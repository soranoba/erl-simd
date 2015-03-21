-module(simd).
-export([foo/1, bar/1, list/1, get_priv/0, init/0]).
-on_load(init/0).

init() ->
    SoName = filename:join([get_priv(), ?MODULE]),
    ok = erlang:load_nif(SoName, 0).

foo(_X) ->
    exit(nif_library_not_loaded).

bar(_Y) ->
    exit(nif_library_not_loaded).

list(_X) ->
    exit(nif_library_not_loaded).

%% @doc Get the priv path of wslogi.
-spec get_priv() -> file:filename().
get_priv() ->
    {ok, Priv} = moyo_application:get_priv_dir(simd),
    Priv.
