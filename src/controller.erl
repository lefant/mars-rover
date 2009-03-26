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
