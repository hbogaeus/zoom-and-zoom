-module(brot).
-author("Nån snubbe & Simon Carlson").

%% API
-compile([debug_info, export_all]).

mandelbrot(C, M) ->
  Z0 = cmplx:new(0, 0),
  I = 0,
  test(I, Z0, C, M).

test(I, Z, C, M) ->
  case I =:= M of                                     %% If max number of iterations is reached, return 0
    true ->
      0;
    false ->
      case cmplx:abs(Z) >= 2 of                       %% If absolute value is equal to or greater than 2, series will diverge
        true ->
          I;                                          %% Return current iteration
        false ->
          ZNew = cmplx:add(cmplx:sqr(Z), C),          %% Calculate Z(n+1)
          test(I + 1, ZNew, C, M)                     %% Recursive call
      end
  end.