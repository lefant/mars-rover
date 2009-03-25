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
 
            visualizer ! {oval,{World1#world.x,World1#world.y},blue},

            Command = get_command(World1),
            gen_tcp:send(Socket,Command),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.



get_command(World) ->
    {SpeedStyle,DiffTurn} = desired_dir(World),
    Turn = correct_turn(
             World#world.turn,
             turn_goal(DiffTurn)),
    Accel = correct_accel(
              World#world.accel,
              accel_goal(
                World#world.speed,
                World#world.maxspeed,
                SpeedStyle)),
    list_to_binary(string:join([Accel,Turn,";"],"")).


desired_dir(World) ->
    {Vx,Vy} =
        coords_to_pov(
          World#world.x,World#world.y,
          -World#world.dir,
          World#world.goal),

    if
        Vx < 0 ->
            if
                Vy == 0 -> Vy1 = 100;
                true -> Vy1 = Vy
            end;
        true ->
            Vy1 = Vy
    end,

    if
        Vy1/Vx < 0 -> Speed = slow;
        Vx > 2*Vy1 -> Speed = fast;
        true -> Speed = normal
    end,
    {Speed,Vy1}.







test_pov_funcs() ->
    {0,0} = test_pov(0,0,0),
    {0,0} = test_pov(0,0,1),

    {1,0} = test_pov(1,0,0),
    {0,1} = test_pov(0,1,0),

    {0,1} = test_pov(1,0,1),
    {-1,0} = test_pov(0,1,1),
    {0,-1} = test_pov(-1,0,1),
    {1,0} = test_pov(0,-1,1),

    {-1.0,-1.0} = coords_to_pov(1,1,0,{0,0}),
    {0.0,0.0} = coords_to_pov(-1,0,0,{-1,0}),

    coords_to_pov(116.633,-149.557,2.2331334012628097,{109.375,-234.375}),
    coords_to_pov(116,-149,2.2331334012628097,{109,-234}),
    coords_to_pov(100,-150,2.2331334012628097,{100,-250}),
    coords_to_pov(0,0,2.2331334012628097,{0,-100}),
    coords_to_pov(0,0,2.2331334012628097,{0,-100}),
    coords_to_pov(0,0,2.2331334012628097,{0,-1}),
    coords_to_pov(0,0,math:pi()/2,{0,-1}),
    coords_to_pov(0,0,math:pi(),{0,-1}),
    ok.
test_pov(X,Y,E) ->
    {X1,Y1} = coords_to_pov(0,0,E*(math:pi()/2),{X,Y}),
    {trunc(X1),trunc(Y1)}.

coords_to_pov(Ox,Oy,Dir,{X,Y}) ->
    X1 = X-Ox,
    Y1 = Y-Oy,
    Sin = math:sin(Dir),
    Cos = math:cos(Dir),
    X2 = X1*Cos-Y1*Sin,
    Y2 = Y1*Cos+X1*Sin,
    %%?LOG({"coords_to_pov: ",{Ox,Oy},Dir,{X,Y},{X1,Y1},{sin,Sin},{cos,Cos},{X2,Y2}}),
    {X2,Y2}.





%%%%% steering

turn_goal(Diff) ->
    HardTresh = 30,
    StraightTresh = 4,
    if
        Diff < (-1*HardTresh) -> "R";
        ((-1*HardTresh) =< Diff) and (Diff < StraightTresh)-> "r";
        ((-1*StraightTresh) =< Diff) and (Diff =< StraightTresh) -> "-";
        (StraightTresh < Diff) and (Diff < HardTresh)-> "l";
        HardTresh < Diff -> "L"
    end.

correct_turn(Cur,Goal) ->
    case Goal of
        Cur -> "";
        "R" -> "r;";
        "L" -> "l;";
        "r" ->
            if
                Cur == "R" -> "l;";
                true -> "r;"
            end;
        "l" ->
            if
                Cur == "L" -> "r;";
                true -> "l;"
            end;
        "-" ->
            case Cur of
                "L" -> "r;";
                "l" -> "r;";
                "R" -> "l;";
                "r" -> "l;"
            end
    end.


accel_goal(CurSpeed,MaxSpeed,Style) ->
    case Style of
        fast ->
            if
                CurSpeed == MaxSpeed -> "-";
                true -> "a"
            end;
        normal ->
            if
                CurSpeed > (4*MaxSpeed)/5 -> "b";
                CurSpeed > (2*MaxSpeed)/3 -> "-";
                true -> "a"
            end;
        slow ->
            if
                CurSpeed > MaxSpeed/2 -> "b";
                CurSpeed > MaxSpeed/3 -> "-";
                true -> "a"
            end
    end.

correct_accel(Cur,Goal) ->
    case Goal of
        Cur -> "";
        "a" -> "a";
        "b" -> "b";
        "-" ->
            case Cur of
                "a" -> "b";
                "b" -> "a"
            end
    end.
