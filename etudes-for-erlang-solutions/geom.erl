%%%-------------------------------------------------------------------
%%% @author jackyhui
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Chapter: 2/3
%%% Functions for calculating areas of geometric shapes.
%%% @end
%%% Created : 19. Feb 2018 14:30
%%%-------------------------------------------------------------------
-module(geom).
-author(jackyhui).

-export([area/3]).

%% @doc Calculates the area of a shape
-type shape() :: rectangle | triangle | ellipse.
-spec area(shape(), number(), number()) -> number().
area(rectangle, Length, Width) ->
    Length * Width;
area(triangle, Base, Height) ->
    Base * Height / 2;
area(ellipse, MajRadius, MinRadius) ->
    math:pi() * MajRadius * MinRadius.
