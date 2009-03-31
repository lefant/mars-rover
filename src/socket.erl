-module(socket).
-export([start/1]).

-include("../include/debug.hrl").


start({Host, Port}) ->
    receive
        {start, {Parser}} ->
            {ok, Socket} = gen_tcp:connect(Host, Port,
                                           [binary,
                                            {packet, 0},
                                            {nodelay, true}]),
            ?LOG("socket:start connection successfully established"),
            loop(Socket, Parser)
    end.

loop(Socket, Parser) ->
    receive
        {tcp, Socket, Bin} ->
            %% ?LOG({"socket: received bin", Bin}),
            case regexp:split(
                   binary_to_list(Bin),
                   ";") of
                {error, Error} ->
                    ?LOG({"socket: error splitting Bin", Bin, Error}),
                    loop(Socket, Parser);
                {ok, MsgList} ->
                    MsgList2 = lists:filter(
                      fun(Msg) -> not (Msg =:= []) end,
                      MsgList),
                    lists:map(
                      fun(Msg) ->
                              case regexp:split(Msg, " ") of
                                  {error, Error} ->
                                      ?LOG({"socket: error splitting Msg",
                                            Msg, Error}),
                                      loop(Socket, Parser);
                                  {ok, List} ->
                                      %% ?LOG({"socket: sending", List}),
                                      Parser ! List
                              end
                      end,
                      MsgList2)
            end,
            loop(Socket, Parser);

        {send, Command} ->
            %% ?LOG({"socket: sending command", Command}),
            gen_tcp:send(Socket, Command),
            loop(Socket, Parser);

        {tcp_closed, Socket} ->
            init:stop()
            %% throw({"fatal: connection to simulator closed"})
    end.
