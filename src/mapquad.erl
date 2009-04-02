-module(mapquad).
-export([start/0]).

-include("../include/debug.hrl").


start() ->
    receive
        {start, {Pathfind, WorldSize, MinSize}} ->
            QuadTree = quadtree:new(trunc(WorldSize/2)),
            Pathfind ! {quadtree, {QuadTree, MinSize}},
            ?LOG({"mapquad entering main loop"}),
            loop(Pathfind, {QuadTree, MinSize})
    end.

loop(Pathfind, {QuadTree, MinSize}) ->
    receive
        {new, Item} ->
            QuadTree1 =
                quadtree:insert_circle(
                  QuadTree,
                  Item,
                  MinSize),
            if
                QuadTree =/= QuadTree1 -> Pathfind ! {quadtree, {QuadTree1, MinSize}};
                true -> void
            end,
            loop(Pathfind, {QuadTree1, MinSize});
        {visualize} ->
            quadtree:visualize(QuadTree, white),
            loop(Pathfind, {QuadTree, MinSize});
        Any ->
            ?LOG({"map:loop received unknown msg", Any}),
            loop(Pathfind, {QuadTree, MinSize})
    end.
