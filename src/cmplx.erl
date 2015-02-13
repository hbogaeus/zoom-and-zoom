-module(cmplx).
-author("Henry Bogaeus & en annan").

-compile([debug_info, export_all]).

new(X, Y) ->
  {cmplx, X, Y}.

add({A, B}, {C, D}) ->
  {cmplx, A + C, B + D}.

sqr({A, B}) ->
  (A * A) - (B * B) + 2 * (A + B).

abs({A, B}) ->
  math:sqrt((A * A) + (B * B)).