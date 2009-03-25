-module(controller).
-export([start/0,test/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/world.hrl").

%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.

-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).


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
