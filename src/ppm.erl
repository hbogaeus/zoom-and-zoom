-module(ppm).

-export([write/2]).

%%% write(Name, Image) The image is a list of rows, each row a list of
%%% tuples {R,G,B}. The RGB values are 0-255. 

write(Name, Image) ->
  Height = length(Image),
  Width = tuple_size(hd((Image))),
  {ok, Fd} = file:open(Name, [write]),
  io:format(Fd, "P6~n", []),
  io:format(Fd, "#~s~n", ["generated by ppm.erl"]),
  io:format(Fd, "~w ~w~n", [Width, Height]),
  io:format(Fd, "255~n", []),
  rows(Image, Fd),
  file:close(Fd).

rows(Rows, Fd) ->
  lists:foreach(fun(R) ->
    Colors = row(R),
    io:put_chars(Fd, Colors)
  end, Rows).

row(Row) ->
  lists:foldr(fun({R,G,B}, A) ->
    [R, G, B | A] end,
    [], Row).
