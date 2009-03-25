-module(socket).
-export([start/2,test/0]).

%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.

-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).


test() ->
    ok.


start({Host,Port},Parser) ->
    {ok, Socket} = gen_tcp:connect(Host,Port,
                                   [binary,
                                    {packet, 0},
                                    {nodelay, true}]),
    ?LOG("connection to simulator successfully established"),
    loop(Socket,Parser).

loop(Socket,Parser) ->
    receive
        {tcp,Socket,Bin} ->
            {ok, Msg} = regexp:split(binary_to_list(Bin)," "),
            Parser ! Msg,
            loop(Socket,Parser);
        {tcp_closed,Socket} ->
            ok
    end.
