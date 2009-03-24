-module(mtm).
-compile(export_all).
%%-export([bench/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/mtm.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/quadtree.hrl").

%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.

-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).






run() ->
    quadtree:visualize_init(),
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
            World1 = parse_init_message(#world{},Msg),
            init_new_round(World1),
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
            World1 = parse_message(World#world{aliens=[]},Msg),

            quadtree:draw_oval({World1#world.x,World1#world.y},blue),

            Command = get_command(World1),
            gen_tcp:send(Socket,Command),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.

init_new_round(World) ->
    quadtree:visualize(World#world.quadtree,white),
    World.


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
    X = World#world.x,
    Y = World#world.y,
    Dir = World#world.dir,
    DesDir = math:atan(Y/X),
    if
        X > 0 -> DesDir1 = DesDir + math:pi();
        true -> DesDir1 = DesDir
    end,

    {Speed,DesDir2} = avoid_obstacle(World,DesDir1),
    Diff = DesDir2 - Dir,
    {Speed,Diff}.




avoid_obstacle(World,Dir) ->
    %% build a list of all dangers, excluding ones
    %% we cannot hit without going at least 100
    Dangers = World#world.craters ++ World#world.boulders ++
        lists:map(
          fun({X,Y,_,_}) ->
                  {X,Y,1}
          end,
          World#world.aliens),
%%    ?LOG({"avoid_obstacles dangers: ",Dangers}),
    Dangers1 =
        lists:filter(
          fun({X,Y,R})->
                  lists:min([abs(X-World#world.x),abs(Y-World#world.y)]) - R < 50
          end,
          Dangers),
%%    ?LOG({"avoid_obstacles prefiltered dangers: ",Dangers1}),

    %% convert all coordinates to ones relative to our vehicle facing
    %% forward on the Y-axis
    Obstacles = 
        lists:map(
          fun({X,Y,R})->
                  {X1,Y1} = coords_to_pov(
                            World#world.x,World#world.y,Dir,
                            {X,Y}),
                  {X1,Y1,R}
          end,
          Dangers1),
%%    ?LOG({"avoid_obstacles obstacles: ",Obstacles}),
    %% filter out everything we cannot hit by going straight ahead

    MinDist =
        lists:min([
                   30,
                   math:sqrt(
                     World#world.x*World#world.x
                     +World#world.y*World#world.y)]),
    Obstacles1 =
        lists:filter(
          fun({X,Y,R})->
%%%                   ?LOG({"avoid_obstacles filtering: ",X,Y,R,abs(Y)-R}),
                  ((X > 0) and (X-R < MinDist)) and (abs(Y) < R+2)
          end,
          Obstacles),
%%%     ?LOG({"avoid_obstacles filtered obstacles: ",Obstacles1}),
%%    ?LOG({"avoid_obstacles: ",length(Dangers),length(Dangers1),length(Obstacles),length(Obstacles1)}),

    SortedObstacles = lists:keysort(2,Obstacles1),
%%%     ?LOG({"avoid_obstacles sorted obstacles: ",SortedObstacles}),

    case SortedObstacles of
        [] -> {fast,Dir};
        [Threat] ->
            {X,Y,R} = Threat,
            if
                Y >= 0 -> S=1;
                Y < 0 -> S=-1
            end,
            Diff = math:atan(S*((R+5)-abs(Y))*100/(X*X)),
            Dir1 = Dir + Diff,
            %% ?LOG({"avoid_obstacles one THREAT: ",{trunc(X),trunc(Y),trunc(R)},Diff}),
            {normal,Dir1};
        [Threat|_] ->
            {X,Y,R} = Threat,
            if
                Y >= 0 -> S=1;
                Y < 0 -> S=-1
            end,
            Diff = math:atan(S*((R+5)-abs(Y))*100/(X*X)),
            Dir1 = Dir + Diff,
            %% ?LOG({"avoid_obstacles one THREAT: ",{trunc(X),trunc(Y),trunc(R)},Diff}),
            {slow,Dir1}

%%%         ThreatList ->
%%%             {X,Y,R} = lists:foldl(
%%%                         fun({X,Y,R},{Wx,Wy,Wr}) ->
%%%                                 Wx1 = lists:min([X,Wx]),
%%%                                 if
%%%                                     abs(Y)-R<abs(Wy)-Wr -> {Wx1,Y,R};
%%%                                     true -> {Wx1,Wy,Wr}
%%%                                 end
%%%                         end,
%%%                         {1000,1000,1},
%%%                         ThreatList),
%%%             if
%%%                 Y >= 0 -> S=1;
%%%                 Y < 0 -> S=-1
%%%             end,
%%%             Diff = math:atan(S*((R+5)-abs(Y))*100/(X*X)),
%%%             Dir1 = Dir + Diff,
%%%             ?LOG({"avoid_obstacles THREAT: ",{trunc(X),trunc(Y),trunc(R)},Diff}),
%%%             {slow,Dir1}

    end.



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
    HardTresh = 0.4,
    StraightTresh = 0.2,
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



%%%%% telemetry message parsing

parse_init_message(World,["I"|List]) ->
    %% I dx dy time-limit min-sensor max-sensor max-speed max-turn max-hard-turn ;
    [Width,Height,TimeLimit,MinSensor,MaxSensor,MaxSpeed,MaxTurn,MaxHardTurn,_] = List,

    QuadTree = #node{
      x=0,
      y=0,
      size=trunc(str2num(Width)/2)
     },

    World#world{
      width=str2num(Width),
      height=str2num(Height),
      timelimit=str2num(TimeLimit),
      minsensor=str2num(MinSensor),
      maxsensor=str2num(MaxSensor),
      maxspeed=str2num(MaxSpeed),
      maxturn=str2num(MaxTurn),
      maxhardturn=str2num(MaxHardTurn),
      quadtree=QuadTree
     }.

parse_message(World,["T"|List]) ->
    %% T time-stamp vehicle-ctl vehicle-x vehicle-y vehicle-dir vehicle-speed objects ;
    [_,VehicleCtl,VehicleX,VehicleY,VehicleDir,VehicleSpeed|ObjectList] = List,
    World1 = World#world{
               turn=string:substr(VehicleCtl, 2),
               accel=string:left(VehicleCtl, 1),
               x=str2num(VehicleX),
               y=str2num(VehicleY),
               dir=(str2num(VehicleDir)/180)*math:pi(),
               speed=str2num(VehicleSpeed)
              },
    parse_object_list(World1,ObjectList);

parse_message(World,["E",Time,Score]) ->
    io:format("EVENT: end of round: time: ~p score: ~p~n",[Time,Score]),
    init_new_round(World);
parse_message(World,["S",Score]) ->
    io:format("EVENT: end of round: score: ~p~n",[Score]),
    init_new_round(World);
parse_message(World,["B",Time]) ->
    io:format("EVENT: Boulder crash: ~p~n",[Time]),
%%%     throw({boulder_bounce,World});
    init_new_round(World);
parse_message(World,["C",Time]) ->
    io:format("EVENT: Crater crash: ~p~n",[Time]),
    throw({crater_crash,World});
parse_message(World,["K",Time]) ->
    io:format("EVENT: Killed by Martian!: ~p~n",[Time]),
    init_new_round(World);
parse_message(World,[Event,Time]) ->
    io:format("EVENT: unknown!!! time: ~p kind of event: ~p~n",[Time,Event]),
    init_new_round(World),
    throw({unknown_event,World});
parse_message(World,Msg) ->
    io:format("unknown message: ~p~n",[Msg]),
    init_new_round(World).
    %% throw({unknown_msg,World,Msg}).


parse_object_list(World,[]) ->
    World;
parse_object_list(World,[Type,X1,Y1,Dir1,Speed1|Rest]) when Type == "m" ->
    X = str2num(X1),
    Y = str2num(Y1),
    Dir = str2num(Dir1),
    Speed = str2num(Speed1),
    AlreadyKnown = lists:member(
                     {X,Y,Dir,Speed},
                     World#world.aliens),
    if 
        AlreadyKnown ->
            parse_object_list(World,Rest);
        true ->
            parse_object_list(
              World#world{
                aliens=[{X,Y,Dir,Speed}|World#world.aliens]
               },Rest)
    end;

parse_object_list(World,[Type,X1,Y1,R1|Rest]) ->
    X = str2num(X1),
    Y = str2num(Y1),
    R = str2num(R1),
    case Type of
        "b" ->
            AlreadyKnown = lists:member(
                             {X,Y,R},World#world.boulders),
            if
                AlreadyKnown -> parse_object_list(World,Rest);
                true ->
                    QuadTree =
                        quadtree:insert_circle(
                          World#world.quadtree,
                          {X,Y,R}),
                    Path = quadtree:astar(
                      QuadTree,
                      {World#world.x,World#world.y},
                      {0,0}),
                    Goal = quadtree:next_subgoal(Path),
                    quadtree:visualize(Path,yellow),
                    quadtree:draw_oval(Goal,green),
                    parse_object_list(
                      World#world{
                        boulders=[{X,Y,R}|World#world.boulders],
                        quadtree=QuadTree,
                        path=Path
                       },Rest)
            end;
        "c" ->
            AlreadyKnown = lists:member(
                             {X,Y,R},
                             World#world.craters),
            if 
                AlreadyKnown -> parse_object_list(World,Rest);
                true ->
                    parse_object_list(
                      World#world{
                        craters=[{X,Y,R}|World#world.craters]
                       },Rest)
            end;
        "h" ->
            %%parse_object_list(World#world{home={X,Y,R}},Rest)
            parse_object_list(World,Rest)
    end.


str2num(Str) ->
    {Num,_} = string:to_float(Str),
    Num.
