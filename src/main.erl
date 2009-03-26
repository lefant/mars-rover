-module(main).
-export([run/0]).
-include("../include/debug.hrl").


run() ->
    visualize:start(),
    ?LOG({"main: visualize server started"}),

    Controller = self(),


    Pathfind = spawn_link(pathfind,start,[]),
    ?LOG({"main: pathfind server spawned",Pathfind}),

    Steer = spawn_link(steer,start,[Controller,Pathfind]),
    ?LOG({"main: steer server spawned",Steer}),

    Map = spawn_link(map,start,[Pathfind,Steer]),
    ?LOG({"main: map server spawned",Map}),

    Parser = spawn_link(parser,start,[Controller,Map]),
    ?LOG({"main: parser server spawned",Parser}),

    Socket = spawn_link(socket,start,[{"localhost",17676},Parser]),
    ?LOG({"main: socket server spawned",Socket}),


    ?LOG({"main: about to start controller server",Controller}),
    controller:start(Socket,Steer).
