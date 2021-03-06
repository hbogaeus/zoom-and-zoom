-module(mandel).
-author("Nån snubbe & Simon Carlson").

%% API
-compile([debug_info, export_all]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
  Trans = fun(W, H) ->
    cmplx:new(X + K * (W - 1), Y - K * (H - 1))
  end,
  rows(Width, Height, Trans, Depth, []).


rows(Width, Height, Trans, Depth, ImageData) ->
  rows(Width, Height, Height, Trans, Depth, ImageData).

rows(_Width, _Height, CurY, _Trans, _Depth, ImageData) when CurY == 0 ->
  ImageData;
rows(Width, Height, CurY, Trans, Depth, ImageData) ->
  Row = calc_row(Width, Width, CurY, Trans, Depth, []),
  rows(Width, Height, CurY - 1, Trans, Depth, [Row | ImageData]).

calc_row(_Width, CurX, _CurY, _Trans, _Depth, RowData) when CurX == 0 ->
  RowData;
calc_row(Width, CurX, CurY, Trans, Depth, RowData) ->
  Complex = Trans(CurX, CurY),
  CurDepth = brot:mandelbrot(Complex, Depth),
  Color = color:convert(CurDepth, Depth),
  calc_row(Width, CurX - 1, CurY, Trans, Depth, [Color | RowData]).


demo() ->
  %small(-0.14, 0.7, 0.6).
  %medium(-0.14, 0.7, 0.6).
  large(-0.14, 0.7, 0.6).

small(X, Y, X1) ->
  Width = 480,
  Height = 270,
  K = (X1 - X) / Width,
  Depth = 512,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("Picture generated in ~w ms~n", [T div 1000]),
  ppm:write("small.ppm", Image).

medium(X, Y, X1) ->
  Width = 960,
  Height = 540,
  K = (X1 - X) / Width,
  Depth = 512,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("Picture generated in ~w ms~n", [T div 1000]),
  ppm:write("medium.ppm", Image).

large(X, Y, X1) ->
  Width = 1920,
  Height = 1080,
  K = (X1 - X) / Width,
  Depth = 512,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("Picture generated in ~w ms~n", [T div 1000]),
  ppm:write("large.ppm", Image).


%  for(CurY = 0; CurY < Height; CurY++)
%      for(CurX = 0; CurX < Height; CurX++)
%         Complex = Trans(CurX, CurY);
%         CurDepth = brot:mandelbrot(Complex, Depth)
%         Colour = color:convert(CurDepth, Depth)
%         lists:append(Row, Color);
%      lists:append(List, Row)
%  return completeList.