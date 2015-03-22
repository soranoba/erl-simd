-module(hipe_impl).
-compile(native).
-compile({hipe, [o3]}).

-export([mul/2, madd/2]).

mul(X, Y) ->
    mul(X, Y, []).

mul([], [], Result) ->
    lists:reverse(Result);
mul([H1 | T1], [H2 | T2], Result) when is_integer(H1), is_integer(H2) ->
    mul(T1, T2, [H1 * H2 | Result]).

madd(X, Y) ->
    madd(X, Y, 0).

madd([], [], R) ->
    R;
madd([H1 | T1], [H2 | T2], R) ->
    madd(T1, T2, R + H1 * H2).
