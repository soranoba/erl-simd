#include "erl_nif.h"

int foo(int x) {
    return x+1;
}

int bar(int y) {
    return y*2;
}

static ERL_NIF_TERM list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM head, tail;
    int val = 0;
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM res;

    tail = argv[0];
    while(!enif_is_empty_list(env, tail)) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_int(env, head, &val)) {
            return enif_make_badarg(env);
        }
        ret = enif_make_list_cell(env, enif_make_int(env, val * 2), ret);
    }
    if (!enif_make_reverse_list(env, ret, &res)) {
        return enif_make_badarg(env);
    }
    return res;
}

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
        return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    if (!enif_get_int(env, argv[0], &y)) {
        return enif_make_badarg(env);
    }
    ret = bar(y);
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"bar", 1, bar_nif},
    {"list", 1, list_nif}
};

ERL_NIF_INIT(simd, nif_funcs, NULL, NULL, NULL, NULL)
