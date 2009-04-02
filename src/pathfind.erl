-module(pathfind).
-export([start/0,test/0]).

-include("../include/debug.hrl").


test() ->
    visualize:start(),

    {Tree, Start, Goal} = quadtree:gen_testquad(),

    Pathfind = spawn_link(pathfind, start, []),
    ?LOG({"pathfind server spawned", Pathfind}),

    Pathfind ! {quadtree, Tree},

    receive
        {Pathfind, {path, Path}} ->
            if
                Path =:= failure ->
                    ?LOG({"test: NO PATH found"}),
                    ok;
                true ->
                    ?LOG({"test: PATH found"}),
                    quadtree:visualize(Path, yellow)
                    %% _ = next_subgoal(Path)
            end
    end,

    visualizer ! {oval, Start, blue},
    visualizer ! {oval, Goal, orange},
    ok.



start() ->
    receive
        {start, {Steer, Home}} ->
            ?LOG({"pathfind started, waiting for initial quadtree"}),
            receive
                {quadtree, QuadTree} ->
                    ?LOG({"pathfind received initial quad, waiting for initial pos"}),
                    receive
                        {pos, Pos} ->
                            newpath(Steer, Home, QuadTree, Pos)
                    end
            end
    end.

loop(Steer, Home, QuadTree, Path, NextNode, CurNode, Pos) ->
    receive
        {pos, Pos1} ->
            CurNode1 = quadtree:find_node(QuadTree, Pos1),
            InNextNode = quadtree:eq_node(CurNode1, NextNode),
            if
                InNextNode ->
                    ?LOG({"pathfind loop: reached current target node, head for next one"}),
                    nextgoal(Steer, Home, QuadTree, Path, Pos1);
                true ->
                    InCurNode = quadtree:eq_node(CurNode1, CurNode),
                    if
                        InCurNode ->
                            loop(Steer, Home, QuadTree, Path, NextNode, CurNode, Pos1);
                        true ->
                            ?LOG({"pathfind loop: ran out of current node into one different from the target, recompute path"}),
                            newpath(Steer, Home, QuadTree, Pos1)
                    end
            end;
        {quadtree, QuadTree1} ->
            ?LOG({"pathfind loop: quadtree received, update it and run newpath"}),
            newpath(Steer, Home, QuadTree1, Pos);
        {reset} ->
            ?LOG({"pathfind loop: reset received, waiting for initial pos"}),
            receive
                {pos,Pos1} ->
                    ?LOG({"pathfind loop: initial pos received"}),
                    newpath(Steer, Home, QuadTree, Pos1)
            end;
        Any ->
            ?LOG({"pathfind loop: unknown msg", Any}),
            loop(Steer, Home, QuadTree, Path, NextNode, CurNode, Pos)
    end.


newpath(Steer, Home, QuadTree, Pos) ->
    Path = quadtree:astar(
              QuadTree,
              Pos,
              Home),
    nextgoal(Steer, Home, QuadTree, Path, Pos).

nextgoal(Steer, Home, QuadTree, Path, Pos) ->
    {Goal, NextNode, Path1} = quadtree:next_subgoal(Path),
    Steer ! {goal, Goal},
    CurNode = quadtree:find_node(QuadTree, Pos),
    loop(Steer, Home, QuadTree, Path1, NextNode, CurNode, Pos).

