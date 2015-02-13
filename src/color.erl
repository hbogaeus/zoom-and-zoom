-module(color).
-author("Nån snubbe & Simon Carlson").

%% API
-compile([debug_info, export_all]).

convert(Depth, Max) ->
  Frac = Depth / Max,
  A = 4 * Frac,
  X = trunc(A),
  Y = trunc(255 * (A - X)),
  case X of
    0 ->
      {Y, 0, 0};
    1 ->
      {255, Y, 0};
    2 ->
      {255 - Y, 255, 0};
    3 ->
      {0, 255, Y};
    4 ->
      {0, 255 - Y, 255}
  end.
