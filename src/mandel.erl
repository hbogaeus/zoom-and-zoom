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

rows(_Width, 0, 0, _Trans, _Depth, ImageData) ->
  ImageData;
rows(Width, Height, 0, Trans, Depth, ImageData) ->
  receive
    {RowData, Height} ->
      rows(Width, Height - 1, 0, Trans, Depth, [RowData | ImageData])
  end;
rows(Width, Height, CurHeight, Trans, Depth, ImageData) ->
  Master = self(),
  spawn_link(fun() -> calc_row(Width, Width, CurHeight, Trans, Depth, ImageData, Master) end),
  rows(Width, Height, CurHeight - 1, Trans, Depth, ImageData).

calc_row(_Width, CurX, Height, _Trans, _Depth, RowData, Master) when CurX == 0 ->
  Master ! {RowData, Height};
calc_row(Width, CurX, Height, Trans, Depth, RowData, Master) ->
  Complex = Trans(CurX, Height),
  CurDepth = brot:mandelbrot(Complex, Depth),
  Color = color:convert(CurDepth, Depth),
  calc_row(Width, CurX - 1, Height, Trans, Depth, [Color | RowData], Master).

demo() ->
  small(-2.6, 1.2, 1.6).

small(X, Y, X1) ->
  Width = 960,
  Height = 540,
  K = (X1 - X) / Width,
  Depth = 1024,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("Picture generated in ~w ms~n", [T div 1000]),
  ppm:write("small.ppm", Image).


%  for(CurY = 0; CurY < Height; CurY++)
%      for(CurX = 0; CurX < Height; CurX++)
%         Complex = Trans(CurX, CurY);
%         CurDepth = brot:mandelbrot(Complex, Depth)
%         Colour = color:convert(CurDepth, Depth)
%         lists:append(Row, Color);
%      lists:append(List, Row)
%  return completeList.