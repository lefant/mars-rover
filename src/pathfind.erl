-module(pathfind).
-export([start/0,test/0]).

-include("../include/debug.hrl").


test() ->
    ?LOG("Debug is enabled"),
    %% dbg:tracer(),
    %% dbg:p(all, call),
    %% dbg:tpl(quadtree, astar, 5, []),

    visualize:start(),

    {Tree, Start, Goal} = quadtree:gen_testquad(),

    Pathfind = spawn_link(pathfind, start, []),
    ?LOG({"pathfind server spawned", Pathfind}),

    Pathfind ! {start, Start},
    Pathfind ! {goal, Goal},
    Pathfind ! {quadtree, Tree},
    Pathfind ! {path, self()},

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
        {start, {Steer}} ->
            receive
                {quadtree, QuadTree} ->
                    loop(Steer, QuadTree, [])
            end
    end.

loop(Steer, QuadTree, Path) ->
    receive
        {nextgoal} ->
            case Path of
                [] ->
                    Steer ! {goal, {0,0}},
                    loop(Steer, QuadTree, []);
                [_] ->
                    Steer ! {goal, {0,0}},
                    loop(Steer, QuadTree, []);
                _ ->
                    {Goal, Path1} = quadtree:next_subgoal(Path),
                    Steer ! {goal, Goal},
                    loop(Steer, QuadTree, Path1)
            end;
        {newpath, {start, Start}, {goal, Goal}} ->
            Path1 = quadtree:astar(
                      QuadTree,
                      Start,
                      Goal),
            loop(Steer, QuadTree, Path1);
        {quadtree, QuadTree1} ->
            ?LOG({"pathfind loop: quadtree event unhandled (should compute new path and send updated subgoal to steer"}),
            loop(Steer, QuadTree1, Path);
        Any ->
            ?LOG({"pathfind loop: unknown msg", Any}),
            loop(Steer, QuadTree, Path)
    end.


