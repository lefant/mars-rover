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
        {reset,endofround} ->
            ?LOG({"controller loop: endofround message received"}),
            Pathfind ! {reset},
            Steer ! {reset},
            loop(Socket, Steer, Pathfind);
        Any ->
            ?LOG({"controller loop: unknown msg",Any}),
            loop(Socket, Steer, Pathfind)
    end.

