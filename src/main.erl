-module(main).
-export([run/0]).
-include("../include/debug.hrl").
-include("../include/world.hrl").

run() ->
    visualize:start(),
    ?LOG({"main: visualize server started"}),

    Main = self(),

    Socket = spawn_link(socket, start, [{"localhost", 17676}]),
    ?LOG({"main: socket server spawned", Socket}),
    Parser = spawn_link(parser, start, []),
    ?LOG({"main: parser server spawned", Parser}),

    Map = spawn_link(map, start, []),
    ?LOG({"main: map server spawned", Map}),
    MapQuad = spawn_link(mapquad, start, []),
    ?LOG({"main: mapquad server spawned", MapQuad}),

    Pathfind = spawn_link(pathfind, start, []),
    ?LOG({"main: pathfind server spawned", Pathfind}),

    Steer = spawn_link(steer, start, []),
    ?LOG({"main: steer server spawned", Steer}),

    Controller = spawn_link(controller, start, []),
    ?LOG({"main: start controller server", Controller}),



    Socket ! {start, {Parser}},
    Parser ! {start, {Main, Controller, Steer, Map}},

    receive
        {world,World} ->
            ok
    end,

    Map ! {start, {MapQuad, Steer}},
    MapQuad ! {start, {Pathfind, World#world.width}},

    Pathfind ! {start,{Steer}},

    Steer ! {start, {Controller}},

    Controller ! {start, {Socket, Steer, Pathfind}},

    

    ok.
