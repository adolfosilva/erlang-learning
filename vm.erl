-module(vm).
-author("Adolfo Silva").
-include("vm.hrl").

%% API
-export([init/0, loop/1, new_vm/0]).

% Returns a vending machine with some default products and some money.
new_vm() -> #vm{
  products = [
    {chocolate, 5, 0.90},
    {coffee, 50, 0.45},
    {tea, 1, 0.55}],
  money = 100.0}.

init() -> spawn(vm, loop, [new_vm()]).

% Main Vending Machine loop.
% Two commands:
%  - list: returns a list of its products;
%  - {price, Product}: returns the price of that Product
%  - {buy, Product, Amount}: client wants to buy a product
loop(State) ->
  receive
    {From, state} -> % for debugging purposes
      From ! {self(), {ok, State}};
    {From, list} ->
      From ! {self(), {ok, [Product || {Product, _,_ } <- State#vm.products]}};
    {From, {price, Product}} ->
      case lists:keyfind(Product, 1, State#vm.products) of
        false -> From ! {self(), {error, "No such product"}};
        {Product, _, Price} -> From ! {self(), {ok, Price}}
      end;
    {From, {buy, Product, Amount}} ->
      % Check if we have the product
      case lists:keyfind(Product, 1, State#vm.products) of
        {_, _, Price} when Amount < Price -> % Check if the Amount given by the user is enough
          From ! {self(), {error, "Not enough money"}};
        {Product, Quantity, Price} ->
          case lists:keyreplace(Product, 1, State#vm.products, {Product, Quantity-1, Price}) of
            [] -> From ! {self(), {error, "No such product"}};
            Products ->
              From ! {self(), {ok, Product}},
              loop(#vm{products = Products, money = State#vm.money})
          end;
        false -> % Couldn't find the product
          From ! {self(), {error, "No such product"}}
      end
  end,
  loop(State).