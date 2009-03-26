%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.

-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).

%% ?LOG("Debug is enabled").

%% dbg:tracer(),
%% dbg:p(all, call),
%% dbg:tpl(quadtree, find_node, 2, []),
