-module(nif_impl).
-export([mul/2, madd/2, badd/1, alpha/3]).
-on_load(init/0).

init() ->
    SoName = filename:join([simd:get_priv(), ?MODULE]),
    ok = erlang:load_nif(SoName, 0).

alpha(_, _, _) ->
    error(nif_library_not_loaded).

badd(_) ->
    error(nif_library_not_loaded).

mul(_, _) ->
    error(nif_library_not_loaded).

madd(_, _) ->
    error(nif_library_not_loaded).
