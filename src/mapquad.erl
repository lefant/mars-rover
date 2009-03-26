-module(mapquad).
-export([start/0]).

-include("../include/debug.hrl").
-include("../include/quadtree.hrl").


start() ->
    receive
        {start, {Pathfind, WorldSize}} ->
            QuadTree = quadtree:new(trunc(WorldSize/2)),
            ?LOG({"mapquad entering main loop"}),
            loop(Pathfind, QuadTree)
    end.

loop(Pathfind, QuadTree) ->
    receive
        {new, Item} ->
            QuadTree1 =
                quadtree:insert_circle(
                  QuadTree,
                  Item),
            %% FIXME: we should only send an update to Pathfind
            %% if the quad actually changed here
            Pathfind ! {quadtree, QuadTree1},
            loop(Pathfind, QuadTree1);
        Any ->
            ?LOG({"map:loop received unknown msg", Any}),
            loop(Pathfind, QuadTree)
    end.

