%% @hidden

-module (erlrc).
-export ([ start/0, stop/0 ]).
-behaviour (application).
-export ([ start/2, stop/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start () ->
  application:start (erlrc).

stop () ->
  application:stop (erlrc).

%-=====================================================================-
%-                        Application callbacks                        -
%-=====================================================================-

start (_Type, _Args) ->
  erlrcsup:start_link ().

stop (_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

%-=====================================================================-
%-                                Tests                                -
%-=====================================================================-

start_test () ->
  try
    ok = code:unstick_dir (code:lib_dir (sasl) ++ "/ebin"),
    erlrc:stop (void),
    ok = erlrc:start ()
  after
    application:stop (erlrc)
  end.

-endif.
