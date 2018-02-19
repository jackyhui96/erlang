%%%-------------------------------------------------------------------
%%% @author jackyhui
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%% Chapter: 2-01
%%% Find the area of a rectangle using it's length and width
%%% @end
%%% Created : 19. Feb 2018 14:30
%%%-------------------------------------------------------------------
-module(geom).
-author(jackyhui).

-export([area/2]).

area(Length, Width) ->
    Length * Width.
