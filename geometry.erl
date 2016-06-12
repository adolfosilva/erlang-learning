-module(geometry).
-export([area/1]).

% Calculates the area of a shape
area({square, Side}) -> Side * Side;
area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius}) -> Pi = 3.14159, Pi * Radius * Radius.
