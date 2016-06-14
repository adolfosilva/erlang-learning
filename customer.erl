%%%-------------------------------------------------------------------
%%% @author Adolfo Silva
%%% @doc
%%% A Vending Machine customer
%%% @end
%%%-------------------------------------------------------------------
-module(customer).
-author("Adolfo Silva").

%% API
-export([start/0, new_customer/0, new_customer/1, loop/1]).

-record(customer, {money = 100.0 :: float()}).

new_customer() -> new_customer(100.0).
new_customer(Amount) -> spawn(?MODULE, loop, [#customer{money = Amount}]).

start() ->
  Vm = vm:init(),
  Customer = new_customer(),
  Customer ! {Vm, {find_price, coffee}},
  ok.

loop(State) ->
  receive
    {Vm, {find_price, Product}} ->
      Vm ! {self(), {price, Product}};
    {_, {ok, {products, Products}}} ->
      io:format("Products: ~p~n", [Products]),
      Products;
    {_, {ok, {price, Price}}} ->
      io:format("Price: ~g~n", [Price]),
      Price;
    {_, {ok, {product, Product}}} ->
      io:format("Product: ~w~n", [Product]),
      Product;
    {_, {error, Msg}} ->
      io:format("Error: ~s~n", [Msg])
  end,
  loop(State).