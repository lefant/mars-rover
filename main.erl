-module(main).
-compile(export_all).
%%-export([bench/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/world.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/quadtree.hrl").





run() ->
    visualize:start(),
    ?LOG({"main: visualize server started"}),

    Controller = spawn_link(controller,start,[]),
    ?LOG({"main: controller server spawned",Controller}),

    Parser = spawn_link(parser,start,[Controller]),
    ?LOG({"main: parser server spawned",Parser}),

    Socket = spawn_link(socket,start,[{"localhost",17676},Parser]),
    ?LOG({"main: socket server spawned",Socket}),

    Socket,
    ok.
