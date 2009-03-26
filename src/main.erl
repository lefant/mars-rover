-module(main).
-export([run/0]).
-include("../include/debug.hrl").


run() ->
    visualize:start(),
    ?LOG({"main: visualize server started"}),

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
    ?LOG({"main: controller server spawned", Controller}),



    Socket ! {start, {Parser}},
    Parser ! {start, {Controller, Steer, Map}},

    receive
        {worldsize,WorldSize} ->
            ok
    end,

    Map ! {start, {MapQuad, Steer}},
    MapQuad ! {start, {Pathfind, WorldSize}},

    Pathfind ! {start,{Steer}},

    Steer ! {start, {Controller}},

    ok.
