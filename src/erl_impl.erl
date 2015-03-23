-module(erl_impl).

-export([mul/2, alpha/3]).

alpha(Alpha, X, Y) ->
    alpha(Alpha, X, Y, <<>>).

alpha(_, <<>>, <<>>, R) ->
    R;
alpha(Alpha, <<H1:32/little-integer, T1/binary>>, <<H2:32/little-integer, T2/binary>>, Result) ->
    alpha(Alpha, T1, T2, <<Result/binary, ((H1 * Alpha + H2 * (256 - Alpha)) div 256):32/little-integer>>).

mul(X, Y) ->
    mul(X, Y, []).

mul([], [], Result) ->
    lists:reverse(Result);
mul([H1 | T1], [H2 | T2], Result) when is_integer(H1), is_integer(H2) ->
    mul(T1, T2, [H1 * H2 | Result]).
