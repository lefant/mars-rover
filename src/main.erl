-module(main).
-compile(export_all).
%%-export([bench/0]).

-include("../include/debug.hrl").
-include("../include/world.hrl").
-include("../include/quadtree.hrl").





run() ->
    visualize:start(),
    ?LOG({"main: visualize server started"}),

    Controller = self(),


    Pathfind = spawn_link(pathfind,start,[]),
    ?LOG({"main: pathfind server spawned",Pathfind}),

    Steer = spawn_link(steer,start,[Controller,Pathfind]),
    ?LOG({"main: steer server spawned",Steer}),


    Parser = spawn_link(parser,start,[Controller,Pathfind,Steer]),
    ?LOG({"main: parser server spawned",Parser}),

    Socket = spawn_link(socket,start,[{"localhost",17676},Parser]),
    ?LOG({"main: socket server spawned",Socket}),


    ?LOG({"main: about to start controller server",Controller}),
    controller:start(Socket,Steer).
