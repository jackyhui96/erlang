%%%-------------------------------------------------------------------
%%% @author jackyhui
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Chapter: 2
%%% Functions for calculating areas of geometric shapes.
%%% @end
%%% Created : 19. Feb 2018 14:30
%%%-------------------------------------------------------------------
-module(geom).
-author(jackyhui).

-export([area/2]).

%% @doc Calculates the area of a rectangle,
%% given the length and width. 
%% Returns the product of its args
-spec area(number(), number()) -> number().
area(Length, Width) ->
    Length * Width.
