-module(socket).
-export([start/2,test/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").


test() ->
    ok.


start({Host,Port},Parser) ->
    {ok, Socket} = gen_tcp:connect(Host,Port,
                                   [binary,
                                    {packet, 0},
                                    {nodelay, true}]),
    ?LOG("connection to simulator successfully established"),
    %% Collector = spawn_link(socket,line_collector,[Parser,[]]),
    loop(Socket,Parser).

loop(Socket,Parser) ->
    receive
        {tcp,Socket,Bin} ->
            %% ?LOG({"socket: received bin",Bin}),
            case regexp:split(
                   binary_to_list(Bin),
                   ";") of
                {error,Error} ->
                    ?LOG({"socket: error splitting Bin",Bin,Error}),
                    loop(Socket,Parser);
                {ok,MsgList} ->
                    MsgList2 = lists:filter(
                      fun(Msg) -> not (Msg == []) end,
                      MsgList),
                    lists:map(
                      fun(Msg) ->
                              case regexp:split(Msg," ") of
                                  {error,Error} ->
                                      ?LOG({"socket: error splitting Msg",Msg,Error}),
                                      loop(Socket,Parser);
                                  {ok,List} ->
                                      ?LOG({"socket: sending",List}),
                                      Parser ! List
                              end
                      end,
                      MsgList2)
            end;

        {tcp_closed,Socket} ->
            ok
    end.

%% line_collector(Parser,Acc) ->
%%     receive
%%         ';' ->
%%             {ok, Msg} = regexp:split(
%%                           lists:reverse(Acc),
%%                           " "),
%%             ?LOG({"socket: collector received ; ->send",Msg}),
%%             Parser ! lists:reverse(Msg),
%%             line_collector(Parser,[]);
%%         C ->
%%             ?LOG({"socket: collector received C",C}),
%%             line_collector(Parser,[C|Acc])
%%     end.
