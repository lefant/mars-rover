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
                            Path1 = quadtree:astar(
                                      QuadTree,
                                      Pos,
                                      Home),
                            {Goal, NextNode, Path2} = quadtree:next_subgoal(Path1),
                            Steer ! {goal, Goal},
                            loop(Steer, Home, QuadTree, Path2, NextNode, Pos)
                    end
            end
    end.

loop(Steer, Home, QuadTree, Path, NextNode, Pos) ->
    receive
        {pos, Pos1} ->
            visualizer ! {oval, Pos1, blue, 3},

            CurNode = quadtree:find_node(QuadTree, Pos1),
            E = quadtree:eq_node(CurNode, NextNode),
            if
                E ->
                    {Goal, NextNode1, Path1} = quadtree:next_subgoal(Path),
                    Steer ! {goal, Goal},
                    loop(Steer, Home, QuadTree, Path1, NextNode1, Pos1);
                true ->
                    loop(Steer, Home, QuadTree, Path, NextNode, Pos1)
            end;
        {newpath} ->
            ?LOG({"pathfind loop: newpath received, call astar, next_subgoal, notify Steer"}),
            Path1 = quadtree:astar(
                      QuadTree,
                      Pos,
                      Home),
            {Goal, NextNode1, Path2} = quadtree:next_subgoal(Path1),
            Steer ! {goal, Goal},
            loop(Steer, Home, QuadTree, Path2, NextNode1, Pos);
        {nextgoal} ->
            ?LOG({"pathfind loop: nextgoal received, call next_subgoal, send Goal to Steer"}),
            {Goal, NextNode1, Path1} = quadtree:next_subgoal(Path),
            Steer ! {goal, Goal},
            loop(Steer, Home, QuadTree, Path1, NextNode1, Pos);
        {quadtree, QuadTree1} ->
            ?LOG({"pathfind loop: quadtree received, update it and send newpath to self"}),
            Path1 = quadtree:astar(
                      QuadTree1,
                      Pos,
                      Home),
            {Goal, NextNode1, Path2} = quadtree:next_subgoal(Path1),
            Steer ! {goal, Goal},
            loop(Steer, Home, QuadTree1, Path2, NextNode1, Pos);
        {reset} ->
            ?LOG({"pathfind loop: reset received, waiting for initial pos"}),
            receive
                {pos,Pos1} ->
                    ?LOG({"pathfind loop: initial pos received"}),
                    Path1 = quadtree:astar(
                              QuadTree,
                              Pos,
                              Home),
                    {Goal, NextNode1, Path2} = quadtree:next_subgoal(Path1),
                    Steer ! {goal, Goal},
                    loop(Steer, Home, QuadTree, Path2, NextNode1, Pos1)
            end;
        Any ->
            ?LOG({"pathfind loop: unknown msg", Any}),
            loop(Steer, Home, QuadTree, Path, NextNode, Pos)
    end.

