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
    connect_simulator(localhost,17676).

connect_simulator(Host,Port) ->
    {ok, Socket} = gen_tcp:connect(Host,Port,
                                   [binary,
                                    {packet, 0},
                                    {nodelay, true}]),
    ?LOG("connection to simulator successfully established"),
    receive
        {tcp,Socket,Bin} ->
            {ok, Msg} = regexp:split(binary_to_list(Bin)," "),
            World1 = parser:parse_init_message(#world{},Msg),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.

receive_loop(Socket,World) ->
    receive
        {tcp,Socket,Bin} ->
            {ok, MsgList} = regexp:split(binary_to_list(Bin),";"),
            {ok, Msg} = regexp:split(
                          lists:last(
                            lists:filter(
                              fun(Item) -> not (Item == []) end,
                              MsgList)),
                          " "),
            World1 = parser:parse_message(World#world{aliens=[]},Msg),
 
            visualizer ! {oval,{World1#world.x,World1#world.y},blue,1},

            Command = steer:get_command(World1),
            gen_tcp:send(Socket,Command),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.
