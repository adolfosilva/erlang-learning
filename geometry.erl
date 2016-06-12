-module(geometry).
-export([area/0, area/1]).

% Calculates the area of a shape
area({square, Side}) -> Side * Side;
area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius}) -> Pi = 3.14159, Pi * Radius * Radius.

area() ->
  receive
    {From, {square, X}} ->
       From ! {self(), X * X};
    {From, {rectangle, X, Y}} ->
       From ! {self(), X * Y};
    {From, {circle, R}} ->
       From ! {self(), 3.14159 * R * R}
  end,
  area().
