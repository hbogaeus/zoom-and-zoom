-module(cmplx).
-author("Nån snubbe & Simon Carlson").

-compile([debug_info, export_all]).

new(X, Y) ->
  {X, Y}.

add({A, B}, {C, D}) ->
  {A + C, B + D}.

sqr({A, B}) ->
  {(A * A) - (B * B), (2 * A * B)}.

abs({A, B}) ->
  math:sqrt((A * A) + (B * B)).