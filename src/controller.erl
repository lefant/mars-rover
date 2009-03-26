-module(controller).
-export([start/0]).

-include("../include/debug.hrl").



start() ->
    receive
        {start,{Socket, Steer, Pathfind}} ->
            ?LOG({"controller entering main loop"}),
            loop(Socket, Steer, Pathfind)
    end.

loop(Socket, Steer, Pathfind) ->
    receive
        {command, Command} ->
            %% ?LOG({"controller:loop passing on", Command}),
            Socket ! {send, Command},
            loop(Socket, Steer, Pathfind);
        Any ->
            ?LOG({"controller loop: unknown msg",Any}),
            loop(Socket, Steer, Pathfind)
    end.



%% parse_message(World,["E",Time,Score]) ->
%%     io:format("EVENT: end of round: time: ~p score: ~p~n",[Time,Score]),
%%     visualizer ! {clear},
%%     quadtree:visualize(World#world.quadtree,white),
%%     Path = quadtree:astar(
%%              World#world.quadtree,
%%              {World#world.x,World#world.y},
%%              {0,0}),
%%     World1 = World#world{
%%                path=Path
%%               },
%%     World1;
