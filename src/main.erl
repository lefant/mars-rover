-module(main).
-export([run/0,start/1]).
-include("../include/debug.hrl").
-include("../include/world.hrl").

run() ->
   start(["localhost", "17676"]).

start([Host, PortStr]) ->
    {Port, _} = string:to_integer(PortStr),


    visualize:start(),
    ?LOG({"main: visualize server started"}),

    Main = self(),

    Socket = spawn_link(socket, start, [{Host, Port}]),
    ?LOG({"main: socket server spawned", Socket}),
    Parser = spawn_link(parser, start, []),
    ?LOG({"main: parser server spawned", Parser}),

    Map = spawn_link(map, start, []),
    ?LOG({"main: map server spawned", Map}),
    Mapquad = spawn_link(mapquad, start, []),
    ?LOG({"main: mapquad server spawned", Mapquad}),

    Pathfind = spawn_link(pathfind, start, []),
    ?LOG({"main: pathfind server spawned", Pathfind}),

    Steer = spawn_link(steer, start, []),
    ?LOG({"main: steer server spawned", Steer}),

    Controller = spawn_link(controller, start, []),
    ?LOG({"main: start controller server", Controller}),



    Socket ! {start, {Parser}},
    Parser ! {start, {Main, Controller, Steer, Map, Pathfind}},

    receive
        {world,World} ->
            ok
    end,
    ?LOG({"main: received initial world"}),

    Map ! {start, {Mapquad, Steer}},
    Mapquad ! {start, {Pathfind, World#world.width}},

    %% Home = World#world.home,
    Home = {0,0,5},
    Pathfind ! {start,{Steer,Home}},

    Steer ! {start, {Controller, Pathfind, World#world.maxspeed}},

    Controller ! {start, {Socket, Steer, Pathfind, Mapquad}},
    

    ok.
