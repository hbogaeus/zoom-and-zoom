-module(helloworld).
-author("Henry Bogaeus").

-compile([debug_info, export_all]).

helloworld() ->
  io:format("Hello World!~n").