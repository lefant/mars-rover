-module(mtm).
-compile(export_all).
%%-export([bench/0]).

-ifdef(debug).
-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
-else.
-define(LOG(Msg), true).
-endif.

-record(world, {
          home = {0.000,0.000,5.000},
          craters = [],
          boulders = [],
          aliens = [],

          width = 100,
          height = 100,
          timelimit = 30000,
          minsensor = 20,
          maxsensor = 40,
          maxspeed = 60,
          maxturn = 20,
          maxhardturn = 60,

          turn = "-",
          accel = "-",
          x = -25.0,
          y = 25,
          dir = 50,
          speed = 0
         }).

test() ->
    ?LOG("Debug is enabled"),
%%%     dbg:tracer(),
%%%     dbg:p(all, call),
%%%     dbg:tpl(mtm, coords_to_pov, 5, []),
%%%     dbg:tpl(mtm, inner_prod, 4, []),
%%%     dbg:tpl(mtm, receive_data, 3, []),
    connect_simulator(localhost,17676).

connect_simulator(Host,Port) ->
    {ok, Socket} = gen_tcp:connect(Host,Port, [binary, {packet, 0}]),
    ?LOG("connection to simulator successfully established"),
    World=#world{},
    receive_loop(Socket,World).

receive_loop(Socket,World) ->
    receive
        {tcp,Socket,Bin} ->
            {ok, Msg} = regexp:split(binary_to_list(Bin)," "),
            World1 = parse_message(World#world{aliens=[]},Msg),
            Command = get_command(World1),
            gen_tcp:send(Socket,Command),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.

get_command(World) ->
    DiffTurn = desired_dir(World),
    TurnState = World#world.turn,
    correct_turn(TurnState,turn_goal(DiffTurn)).
%%%     steer(Dir,Ctl).

desired_dir(World) ->
    X = World#world.x,
    Y = World#world.y,
    Dir = World#world.dir,
    %%DesDir = (math:atan(Y/X)/math:pi())*180,
    DesDir = math:atan(Y/X),
    if
        X > 0 -> DesDir1 = DesDir + math:pi();
        true -> DesDir1 = DesDir
    end,

    avoid_obstacle(World,DesDir1),
    Diff = DesDir1 - Dir,
    %%?LOG({"desire: ",X,Y,Dir,DesDir,Diff}),
    %%check_rectangle(World,(DesDir1/180)*math:pi()),
%%%     check_rectangle(World,DesDir1),
    Diff.


avoid_obstacle(World,Dir) ->
    %% build a list of all dangers, excluding ones
    %% we cannot hit without going at least 100
    Dangers =
        lists:filter(
          fun({X,Y,R})->
                  lists:min([abs(X-World#world.x),abs(Y-World#world.y)]) - R > 100
          end,
          World#world.craters ++ World#world.boulders),
    %% convert all coordinates to ones relative to our vehicle facing
    %% forward on the Y-axis
    Obstacles = 
        lists:map(
          fun({X,Y,R})->
                  {X1,Y1} = coords_to_pov(
                            World#world.x,World#world.y,World#world.dir,
                            {X,Y}),
                  {X1,Y1,R}
          end,
          Dangers),
    ?LOG({"avoid_obstacles: ",length(Dangers),length(Obstacles)}),
    Dir1 = Dir,
    Dir1.



coords_to_pov(Ox,Oy,Dir,{X,Y}) ->
    X1 = X-Ox,
    Y1 = Y-Oy,
    X2 = X1*math:sin(Dir) - Y1*math:cos(Dir),
    Y2 = X1*math:cos(Dir) + Y1*math:sin(Dir),
    ?LOG({"coords_to_pov: ",{Ox,Oy},Dir,{X,Y},{X1,Y1},{X2,Y2}}),
    {X2,Y2}.


%% check_rectangle(World,Dir) ->
%%     X = World#world.x,
%%     Y = World#world.y,
%%     R = 0.5,
%%     L = 10,
%%     Vx = math:sin(Dir),
%%     Vy = math:cos(Dir),
%%     B1X = X-Vy*R,
%%     B1Y = Y+Vx*R,
%%     B2X = X+Vy*R,
%%     B2Y = Y-Vx*R,
%%     F1X = B1X+Vx*L,
%%     F1Y = B1Y+Vy*L,
%%     F2X = B2X+Vx*L,
%%     F2Y = B2Y+Vy*L,
%%     ok.


turn_goal(Diff) ->
    HardTresh = 0.3,
    StraightTresh = 0.1,
    if
        Diff < (-1*HardTresh) -> "R";
        ((-1*HardTresh) =< Diff) and (Diff < StraightTresh)-> "r";
        ((-1*StraightTresh) =< Diff) and (Diff =< StraightTresh) -> "-";
        (StraightTresh < Diff) and (Diff < HardTresh)-> "l";
        HardTresh < Diff -> "L"
    end.

correct_turn(Cur,Goal) ->
    case Cur of
        Goal -> C = "a;";
        "R" -> C = "al;";
        "L" -> C = "ar;";
        "r" ->
            if
                Goal == "R" -> C = "ar;";
                true -> C = "al;"
            end;
        "l" ->
            if
                Goal == "L" -> C = "al;";
                true -> C = "ar;"
            end;
        "-" ->
            case Goal of
                "L" -> C = "al;";
                "l" -> C = "al;";
                "R" -> C = "ar;";
                "r" -> C = "ar;"
            end
    end,
    list_to_binary(C).







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

parse_message(World,["I"|List]) ->
    %% I dx dy time-limit min-sensor max-sensor max-speed max-turn max-hard-turn ;
    [Width,Height,TimeLimit,MinSensor,MaxSensor,MaxSpeed,MaxTurn,MaxHardTurn,_] = List,
    World#world{
      width=str2num(Width),
      height=str2num(Height),
      timelimit=str2num(TimeLimit),
      minsensor=str2num(MinSensor),
      maxsensor=str2num(MaxSensor),
      maxspeed=str2num(MaxSpeed),
      maxturn=str2num(MaxTurn),
      maxhardturn=str2num(MaxHardTurn)
     };
parse_message(World,["E",Time,Score,";"]) ->
    ?LOG({"parse_message: END ",Time,Score}),
    io:format("end of round: time: ~p score: ~p~n",[Time,Score]),
    World;
parse_message(World,[Time,Event,";"]) ->
    ?LOG({"parse_message: EVENT ",Time,Event}),
    io:format("special event: time: ~p kind of event: ~p~n",[Time,Event]),
    World.


parse_object_list(World,[";"]) ->
    World;
parse_object_list(World,[Type,X,Y,Dir,Speed|Rest]) when Type == "m" ->
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
                    parse_object_list(
                      World#world{
                        boulders=[{X,Y,R}|World#world.boulders]
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
