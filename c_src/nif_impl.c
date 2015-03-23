#include <erl_nif.h>
#include <math.h>
#include <arpa/inet.h>

static ERL_NIF_TERM alpha_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary bin1, bin2;

    ERL_NIF_TERM term;
    unsigned short a;
    unsigned char *m4;
    int i;

    if (!enif_get_int(env, argv[0], &i)
        || !enif_inspect_binary(env, argv[1], &bin1)
        || !enif_inspect_binary(env, argv[2], &bin2)) {
        return enif_make_badarg(env);
    }
    a = (unsigned short)i;

    m4 = (unsigned char*)enif_make_new_binary(env, bin1.size, &term);

    for (i = 0; i < bin1.size; ++i) {
        m4[i] = (bin1.data[i] * a + bin2.data[i] * (256 - a)) >> 8;
    }

    return term;
}

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
    double d2 = floor(d);
    if (d == d2) {
        return enif_make_long(env, (long)d2);
    }
    return enif_make_double(env, d);
}

static ERL_NIF_TERM madd_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM head1, tail1, head2, tail2;
    double val1, val2, r = 0.0;

    tail1 = argv[0], tail2 = argv[1];

    while (1) {
        if (enif_is_empty_list(env, tail1) && enif_is_empty_list(env, tail2)) {
            break;
        } else if (!enif_get_list_cell(env, tail1, &head1, &tail1)
                   || !enif_get_list_cell(env, tail2, &head2, &tail2)
                   || !enif_get_number(env, head1, &(val1))
                   || !enif_get_number(env, head2, &(val2))) {
            return enif_make_badarg(env);
        }
        r += val1 * val2;
    }
    return enif_make_number(env, r);
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

    while (1) {
        val1 = enif_get_list_cell(env, tail1, &head1, &tail1);
        val2 = enif_get_list_cell(env, tail2, &head2, &tail2);
        if (!val1) {
            if (!val2) {
                break;
            } else {
                goto badarg;
            }
        }

        if (!enif_get_int(env, head1, &val1)
            || !enif_get_int(env, head2, &val2)) {
            goto badarg;
        }
        r[i++] = enif_make_int(env, val1 * val2);
    }
    /* if (!enif_is_empty_list(env, tail1) */
    /*     || !enif_is_empty_list(env, tail2)) { */
    /*     goto badarg; */
    /* } */
    head1 = enif_make_list_from_array(env, r, i);
    enif_free(r);
    return head1;

 badarg:
    enif_free(r);
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] = {
    {"alpha", 3, alpha_nif},
    {"madd", 2, madd_nif},
    {"mul", 2, mul_nif},
    {"badd", 1, badd_nif}
};

ERL_NIF_INIT(nif_impl, nif_funcs, NULL, NULL, NULL, NULL)
