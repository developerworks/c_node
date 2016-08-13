-module(c_node_short).
-export([foo/1, bar/1]).

foo(X) ->
  call_cnode({foo, X}).
bar(Y) ->
  call_cnode({bar, Y}).

call_cnode(Msg) ->
  {any, c1@localhost} ! {call, self(), Msg},
  receive
    {cnode, Result} ->
      io:format("Result: ~p~n", [Result])
  end.
