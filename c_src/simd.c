#include "erl_nif.h"
#include <immintrin.h>
#include <math.h>

#define ALIGN32 __attribute((aligned(32)))

static ERL_NIF_TERM badd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin;
    ERL_NIF_TERM term;
    unsigned int *d, *r;
    int i, max;

    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }
    r = (unsigned int*)enif_make_new_binary(env, bin.size, &term);
    d = (unsigned int*)bin.data;
    max = bin.size * sizeof(unsigned char) / sizeof(unsigned int);

    for (i = 0; i < max; ++i) {
        r[i] = d[i] * 2;
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
    {"foo", 1, foo_nif},
    {"madd", 2, madd_nif},
    {"mul", 2, mul_nif}
};

ERL_NIF_INIT(simd, nif_funcs, NULL, NULL, NULL, NULL)
