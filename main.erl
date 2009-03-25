-module(main).
-compile(export_all).
%%-export([bench/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/world.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/quadtree.hrl").





run() ->
    visualize:start(),
    ?LOG({"main: visualize started"}),

    Controller = spawn_link(controller,start,[]),
    ?LOG({"main: controller spawned"}),

    Parser = spawn_link(parser,start,[Controller]),
    ?LOG({"main: parser spawned"}),

    Socket = spawn_link(socket,start,[{"localhost",17676},Parser]),
    ?LOG({"main: socket spawned"}),

    Socket,
    ok.
