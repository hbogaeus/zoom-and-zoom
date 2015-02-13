-module(mandel).
-author("Nån snubbe & Simon Carlson").

%% API
-compile([debug_info, export_all]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
  Trans = fun(W, H) ->
    cmplx:new(X + K * (W - 1), Y - K * (H - 1))
  end,
  rows(Width, Height, Trans, Depth, []).


rows(Width, Height, Trans, Depth, List) ->
  case Height of
    0 ->
      case Width of
        0 ->
          Pixel = Trans(Width, Height),
          Brot = brot:mandelbrot(Pixel, Depth),
          Color = color:convert(Brot, Depth),
          [Color | List];
        _Val ->
          Pixel = Trans(Width, Height),
          Brot = brot:mandelbrot(Pixel, Depth),
          Color = color:convert(Brot, Depth),
          rows(Width - 1, Height, Trans, Depth, [Color | List])
      end;
    _Val1 ->
      case Width of
        0 ->
          Pixel = Trans(Width, Height),
          Brot = brot:mandelbrot(Pixel, Depth),
          Color = color:convert(Brot, Depth),
          rows(960, Height - 1, Trans, Depth, [Color | List]);
        _Val2 ->
          Pixel = Trans(Width, Height),
          Brot = brot:mandelbrot(Pixel, Depth),
          Color = color:convert(Brot, Depth),
          rows(Width - 1, Height, Trans, Depth, [Color | List])
      end
  end.

demo() ->
  small(-2.6, 1.2, 1.6).

small(X, Y, X1) ->
  Width = 5,
  Height = 3,
  K = (X1 - X) / Width,
  Depth = 64,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("Picture generated in ~w ms~n", [T div 1000]),
  ppm:write("small.ppm", Image).
