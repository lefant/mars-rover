-module(controller).
-export([start/2,test/0]).

-include("../include/debug.hrl").
-include("../include/world.hrl").


test() ->
    ok.


start(Socket,Steer) ->
    init(Socket,Steer),
    ok.

init(Socket,Steer) ->
    receive
        {world_ready} ->
            loop(Socket,Steer)
    end.

loop(Socket,Steer) ->
    receive
        {reinit} ->
            ?LOG({"controller loop: reinit"}),
            init(Socket,Steer);
        Any ->
            ?LOG({"controller loop: unknown msg",Any}),
            loop(Socket,Steer)
    end.
