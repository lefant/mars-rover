-module(main).
-compile(export_all).
%%-export([bench/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/world.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/quadtree.hrl").

%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.

-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).




run() ->
    visualize:start(),
    Controller = spawn_link(controller,start,[]),
    Parser = spawn_link(parser,start,[Controller]),
    Socket = spawn_link(socket,start,[{"localhost",17676},Parser]),
    Socket,
    ok.
