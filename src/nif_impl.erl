-module(nif_impl).
-export([mul/2]).
-on_load(init/0).

init() ->
    SoName = filename:join([simd:get_priv(), ?MODULE]),
    ok = erlang:load_nif(SoName, 0).

mul(_, _) ->
    error(nif_library_not_loaded).
