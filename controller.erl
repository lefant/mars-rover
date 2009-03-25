-module(controller).
-export([start/0,test/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/world.hrl").


test() ->
    ok.


start() ->
    init(),
    ok.

init() ->
    receive
        {world_ready} ->
            loop()
    end.

loop() ->
    receive
        {reinit} ->
            ?LOG({"controller loop: reinit"}),
            init();
        Any ->
            ?LOG({"controller loop: unknown msg",Any}),
            loop()
    end.
