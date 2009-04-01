-module(controller).
-export([start/0]).

-include("../include/debug.hrl").



start() ->
    receive
        {start, Pids} ->
            ?LOG({"controller entering main loop"}),
            loop(Pids)
    end.

loop({Socket, Steer, Pathfind, Mapquad}=Pids) ->
    receive
        {command, Command} ->
            %% ?LOG({"controller:loop passing on", Command}),
            Socket ! {send, Command},
            loop(Pids);
        {reset, endofround, TimeStamp} ->
            ?LOG({"controller loop: endofround message received", TimeStamp}),
            Steer ! {reset},
            Pathfind ! {reset},
            visualizer ! {clear},
            Mapquad ! {visualize},
            loop(Pids);
        Any ->
            ?LOG({"controller loop: unknown msg", Any}),
            loop(Pids)
    end.

