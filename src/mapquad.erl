-module(mapquad).
-export([start/0]).

-include("../include/debug.hrl").


start() ->
    receive
        {start, {Pathfind, WorldSize}} ->
            QuadTree = quadtree:new(trunc(WorldSize/2)),
            Pathfind ! {quadtree, QuadTree},
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
            %% NEEDS TESTING
            if
                QuadTree =/= QuadTree1 -> Pathfind ! {quadtree, QuadTree1};
                true -> void
            end,
            loop(Pathfind, QuadTree1);
        Any ->
            ?LOG({"map:loop received unknown msg", Any}),
            loop(Pathfind, QuadTree)
    end.
