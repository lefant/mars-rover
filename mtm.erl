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

          vehiclectl = "a-",
          vehiclex = -25.0,
          vehicley = 25,
          vehicledir = 50,
          vehiclespeed = 0
         }).

test() ->
    ?LOG("Debug is enabled"),
%%%     dbg:tracer(),
%%%     dbg:p(all, call),
%%%     dbg:tpl(mtm, desired_dir, 1, []),
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
            gen_tcp:send(Socket,get_command(World1)),
            receive_loop(Socket,World1);
        {tcp_closed,Socket} ->
            ok
    end.
    

desired_dir(World) ->
    X = World#world.vehiclex,
    Y = World#world.vehicley,
    Dir = World#world.vehicledir,
    DesDir = (math:atan(Y/X)/math:pi())*180,
    if
        X > 0 -> DesDir1 = DesDir + 180;
        true -> DesDir1 = DesDir
    end,
    Diff = DesDir1 - Dir,
    ?LOG({"desire: ",X,Y,Dir,DesDir,Diff}),
    Diff.

get_command(World) ->
    Dir = desired_dir(World),
    Ctl = string:substr(World#world.vehiclectl, 2),
    %%?LOG({"Ctl1: ",Ctl1}),
    case Ctl of
        "L" ->
            if
                Dir < 10 ->
                    Command = list_to_binary("ar;");
                true ->
                    Command = list_to_binary("a;")
            end;
        "l" ->
            if
                Dir < 0 ->
                    Command = list_to_binary("ar;");
                Dir > 10 ->
                    Command = list_to_binary("al;");
                true ->
                    Command = list_to_binary("a;")
            end;
        "-" ->
            if 
                Dir < 0 ->
                    Command = list_to_binary("ar;");
                Dir > 0 ->
                    Command = list_to_binary("al;");
                true ->
                    Command = list_to_binary("a;")
            end;
        "r" ->
            if 
                Dir < -10 ->
                    Command = list_to_binary("ar;");
                Dir > 0 ->
                    Command = list_to_binary("al;");
                true ->
                    Command = list_to_binary("a;")
            end;
        "R" ->
            if 
                Dir > -10 ->
                    Command = list_to_binary("al;");
                true ->
                    Command = list_to_binary("a;")
            end
    end,
    ?LOG({"steer: ",Ctl,Command,Dir}),
    Command.

parse_message(World,["T"|List]) ->
    %% T time-stamp vehicle-ctl vehicle-x vehicle-y vehicle-dir vehicle-speed objects ;
    [_,VehicleCtl,VehicleX,VehicleY,VehicleDir,VehicleSpeed|ObjectList] = List,
    World1 = World#world{
      vehiclectl=VehicleCtl,
      vehiclex=str2num(VehicleX),
      vehicley=str2num(VehicleY),
      vehicledir=str2num(VehicleDir),
      vehiclespeed=str2num(VehicleSpeed)
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

parse_object_list(World,[Type,X,Y,R|Rest]) ->
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
