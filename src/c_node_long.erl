-module(c_node_long).
-export([foo/1, bar/1]).

foo(X) ->
  call_cnode({foo, X}).
bar(Y) ->
  call_cnode({bar, Y}).

call_cnode(Msg) ->
  {any, 'cnode@idril.du.uab.ericsson.se'} ! {call, self(), Msg},
  receive
    {cnode, Result} ->
      io:format("Result: ~p~n", [Result])
  end.
