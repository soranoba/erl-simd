#include "erl_nif.h"

static ERL_NIF_TERM mul_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM head1, tail1, head2, tail2;
    int val1, val2;
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM res;

    tail1 = argv[0], tail2 = argv[1];
    while(!enif_is_empty_list(env, tail1)
          || !enif_is_empty_list(env, tail2)) {
        if (!enif_get_list_cell(env, tail1, &head1, &tail1)
            || !enif_get_list_cell(env, tail2, &head2, &tail2)) {
            return enif_make_badarg(env);
        }
        if (!enif_get_int(env, head1, &val1)
            || !enif_get_int(env, head2, &val2)) {
            return enif_make_badarg(env);
        }
        ret = enif_make_list_cell(env, enif_make_int(env, val1 * val2), ret);
    }
    if (!enif_make_reverse_list(env, ret, &res)) {
        return enif_make_badarg(env);
    }
    return res;
}

static ErlNifFunc nif_funcs[] = {
    {"mul", 2, mul_nif}
};

ERL_NIF_INIT(nif_impl, nif_funcs, NULL, NULL, NULL, NULL)
