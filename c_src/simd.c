#include "erl_nif.h"
#include <immintrin.h>
#include <math.h>
#include <stdio.h>

#define ALIGN32 __attribute((aligned(32)))

static ERL_NIF_TERM alpha_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin1, bin2;

    ERL_NIF_TERM term;
    unsigned short a;
    __m128i *m1, *m2, *m4;
    __m128i alpha, r_alpha, s1, s2, s11, s22, sum1, sum2,res1, res2, res;
    int i, max;

    if (!enif_get_int(env, argv[0], &i)
        || !enif_inspect_binary(env, argv[1], &bin1)
        || !enif_inspect_binary(env, argv[2], &bin2)) {
        return enif_make_badarg(env);
    }
    a = (unsigned short)i;
    alpha   = _mm_set1_epi16(a);
    r_alpha = _mm_set1_epi16(256 - a);

    m4 = (__m128i*)enif_make_new_binary(env, bin1.size, &term);
    m1 = (__m128i*)bin1.data;
    m2 = (__m128i*)bin2.data;

    max = bin1.size * sizeof(char) / sizeof(__m128i);
    for (i = 0; i < max; ++i) {
        {
            s1 = _mm_cvtepu8_epi16(m1[i]);
            s2 = _mm_cvtepu8_epi16(m2[i]);

            s11 = _mm_mullo_epi16(s1, alpha);
            s22 = _mm_mullo_epi16(s2, r_alpha);

            sum1 = _mm_adds_epu16(s11, s22);
            res1 = _mm_srli_epi16(sum1, 8);
        }
        {
            s1 = _mm_cvtepu8_epi16(_mm_srli_si128(m1[i], 8));
            s2 = _mm_cvtepu8_epi16(_mm_srli_si128(m2[i], 8));

            s11 = _mm_mullo_epi16(s1, alpha);
            s22 = _mm_mullo_epi16(s2, r_alpha);

            sum2 = _mm_adds_epu16(s11, s22);
            res2 = _mm_srli_epi16(sum2, 8);
        }

        res = _mm_packus_epi16(res1, res2);
        _mm_storeu_si128(&(m4[i]), res);
    }

    return term;
}

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    double a;
    if (!enif_get_double(env, argv[0], &a)) {
        return enif_make_badarg(env);
    }
    return enif_make_double(env,a + 1);
}

static int enif_get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* dp) {
    if (!enif_get_double(env, term, dp)) {
        int i;
        if (!enif_get_int(env, term, &i)) {
            return 0;
        }
        *dp = i;
    }
    return 1;
}

static ERL_NIF_TERM enif_make_number(ErlNifEnv* env, double d) {
    if (d == floor(d)) {
        return enif_make_long(env, (long)d);
    }
    return enif_make_double(env, d);
}

static ERL_NIF_TERM madd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM head1, tail1, head2, tail2;
    __m256d *m1, *m2, m4 = {0,0,0,0};
    double ret = 0;
    double d1[sizeof(__m256d) / sizeof(double)] ALIGN32;
    double d2[sizeof(__m256d) / sizeof(double)] ALIGN32;
    double d4[sizeof(__m256d) / sizeof(double)] ALIGN32 = {0};
    unsigned int i = 0, j = 0;

    tail1 = argv[0], tail2 = argv[1];

    while(i == j) {
        for (i = 0; i < sizeof(__m256d) / sizeof(double); ++i) {
            if (enif_is_empty_list(env, tail1) && enif_is_empty_list(env, tail2)) {
                break;
            } else if (!enif_get_list_cell(env, tail1, &head1, &tail1)
                       || !enif_get_list_cell(env, tail2, &head2, &tail2)
                       || !enif_get_number(env, head1, &(d1[i]))
                       || !enif_get_number(env, head2, &(d2[i]))) {
                return enif_make_badarg(env);
            }
        }
        for (j = i; j < sizeof(__m256d) / sizeof(double); ++j) {
            d1[j] = d2[j] = 0.0;
        }
        m1 = (__m256d*)d1, m2 = (__m256d*)d2;
        m4 = _mm256_fmadd_pd(m1[0], m2[0], m4);
    }

    _mm256_store_pd(d4, m4);
    for (j = 0; j < sizeof(__m256d) / sizeof(double); ++j) {
        ret += d4[j];
    }
    return enif_make_number(env, ret);
}

static ERL_NIF_TERM mul_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM head1, tail1, head2, tail2;
    int val1, val2;
    unsigned len;
    ERL_NIF_TERM* r;
    int i = 0;

    tail1 = argv[0], tail2 = argv[1];
    if (!enif_get_list_length(env, argv[1], &len)) {
        return enif_make_badarg(env);
    }
    r = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * len);

    while (enif_get_list_cell(env, tail1, &head1, &tail1)
           && enif_get_list_cell(env, tail2, &head2, &tail2)) {

        if (!enif_get_int(env, head1, &val1)
            | !enif_get_int(env, head2, &val2)) {
            return enif_make_badarg(env);
        }
        r[i++] = enif_make_int(env, val1 * val2);
    }
    if (!enif_is_empty_list(env, tail1)
        || !enif_is_empty_list(env, tail2)) {
        enif_free(r);
        return enif_make_badarg(env);
    }
    head1 = enif_make_list_from_array(env, r, i);
    enif_free(r);
    return head1;
}

static ErlNifFunc nif_funcs[] = {
    {"alpha", 3, alpha_nif},
    {"foo", 1, foo_nif},
    {"madd", 2, madd_nif},
    {"mul", 2, mul_nif}
};

ERL_NIF_INIT(simd, nif_funcs, NULL, NULL, NULL, NULL)
