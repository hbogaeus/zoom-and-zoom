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
  rows(Width, Height, 0, 0, Trans, Depth, [], List).

rows(Width, Height, CurX, CurY, Trans, Depth, Row, List) ->
%  for(CurY = 0; CurY < Height; CurY++)
%      for(CurX = 0; CurX < Height; CurX++)
%         Complex = Trans(CurX, CurY);
%         CurDepth = brot:mandelbrot(Complex, Depth)
%         Colour = color:convert(CurDepth, Depth)
%         lists:append(Row, Color);
%      lists:append(List, Row)
%  return completeList.

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
