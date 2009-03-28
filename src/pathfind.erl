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
                Path == failure ->
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
                        {pos,Pos} ->
                            self() ! {newpath},
                            loop(Steer, Home, QuadTree, [], Pos)
                    end
            end
    end.

loop(Steer, Home, QuadTree, Path, Pos) ->
    receive
        {pos, Pos1} ->
            case Path of
                [CurPathNode|_] ->
                    CurNode = quadtree:find_node(QuadTree, Pos1),
                    E = quadtree:eq_node(CurNode, CurPathNode),
                    if
                        not E ->
                            self() ! {newpath};
                        true ->
                            ok
                    end;
                _ ->
                    ok
            end,
            loop(Steer, Home, QuadTree, Path, Pos1);
        {newpath} ->
            Path1 = quadtree:astar(
                      QuadTree,
                      Pos,
                      Home),
            self() ! {nextgoal},
            loop(Steer, Home, QuadTree, Path1, Pos);
        {nextgoal} ->
            case Path of
                [] ->
                    {X,Y,_} = Home,
                    Goal = {X,Y},
                    Path1 = [];
                [_] ->
                    {X,Y,_} = Home,
                    Goal = {X,Y},
                    Path1 = [];
                _ ->
                    {Goal, Path1} = quadtree:next_subgoal(Path)
            end,
            Steer ! {goal, Goal},
            loop(Steer, Home, QuadTree, Path1, Pos);
        {quadtree, QuadTree1} ->
            %% ?LOG({"pathfind loop: quadtree event unhandled (should compute new path and send updated subgoal to steer (but need Start point to do so"}),
            self() ! {newpath},
            loop(Steer, Home, QuadTree1, Path, Pos);
        {reset} ->
            receive
                {pos,Pos1} ->
                    self() ! {newpath},
                    loop(Steer, Home, QuadTree, [], Pos1)
            end;
        Any ->
            ?LOG({"pathfind loop: unknown msg", Any}),
            loop(Steer, Home, QuadTree, Path, Pos)
    end.

    

