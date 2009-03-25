-module(socket).
-export([start/2,test/0,line_collector/2]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").


test() ->
    ok.


start({Host,Port},Parser) ->
    {ok, Socket} = gen_tcp:connect(Host,Port,
                                   [binary,
                                    {packet, 0},
                                    {nodelay, true}]),
    ?LOG("connection to simulator successfully established"),
    Collector = spawn_link(socket,line_collector,[Parser,[]]),
    loop(Socket,Collector).

loop(Socket,Collector) ->
    receive
        {tcp,Socket,Bin} ->
            {ok, Msg} = regexp:split(binary_to_list(Bin)," "),
            Collector ! Msg,
            loop(Socket,Collector);
        {tcp_closed,Socket} ->
            ok
    end.

line_collector(Parser,Acc) ->
    receive
        ";" ->
            Parser ! lists:reverse(Acc),
            line_collector(Parser,[]);
        C ->
            line_collector(Parser,[C|Acc])
    end.
