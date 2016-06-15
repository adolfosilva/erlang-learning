-module(calculator).
-export([start/0, loop/1, push/1, add/0, sub/0]).

start() ->
  register(?MODULE, spawn(calculator, loop, [[]])).

push(N) -> rpc({push, N}).
add() -> rpc(add).
sub() -> rpc(sub).

rpc(Operation) ->
  Tag = make_ref(),
  calculator ! {self(), Tag, Operation},
  receive
    {Tag, {error, Msg}} -> Msg;
    {Tag, Reply} -> Reply
  end. 

loop(Stack) ->
  receive
    {From, Tag, {push, N}} ->
      From ! {Tag, ack},
      loop([N | Stack]);
    {From, Tag, add} ->
      pop_two_and_apply(Stack, From, Tag, fun(X,Y) -> X+Y end);
    {From, Tag, sub} ->
      pop_two_and_apply(Stack, From, Tag, fun(X,Y) -> X-Y end)
  end.

pop([]) -> [];
pop([H|T]) -> {H, T}.

pop_two_and_apply(Stack, From, Tag, F) ->
  case pop(Stack) of
    [] ->
      From ! {Tag, {error, "No values on the stack"}},
      loop(Stack);
    {X, L1} ->
      case pop(L1) of
        [] ->
          From ! {Tag, {error, "No enough values on the stack"}},
          loop(Stack);
        {Y, L2} ->
          From ! {Tag, F(Y,X)},
          loop(L2)
      end
  end.
