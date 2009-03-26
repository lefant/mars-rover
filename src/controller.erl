-module(controller).
-export([start/0]).

-include("../include/debug.hrl").



start() ->
    receive
        {start,{Socket, Steer, Pathfind}} ->
            loop(Socket, Steer, Pathfind)
    end.

loop(Socket, Steer, Pathfind) ->
    receive
        {command, Command} ->
            Socket ! {send, Command};
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
