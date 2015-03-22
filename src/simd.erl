-module(simd).
-export([mul/2, madd/2, foo/1, get_priv/0]).
-on_load(init/0).

init() ->
    SoName = filename:join([get_priv(), ?MODULE]),
    ok = erlang:load_nif(SoName, 0).

foo(_X) ->
    exit(nif_library_not_loaded).

madd(_X, _Y) ->
    exit(nif_library_not_loaded).

mul(_X, _Y) ->
    exit(nif_library_not_loaded).

%% @doc Get the priv path of wslogi.
-spec get_priv() -> file:filename().
get_priv() ->
    {ok, Priv} = moyo_application:get_priv_dir(simd),
    Priv.
