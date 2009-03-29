-module(parser).
-export([start/0]).

-include("../include/debug.hrl").
-include("../include/world.hrl").
-include("../include/rover.hrl").



start() ->
    receive
        {start, {Main, Controller, Steer, Map, Pathfind}} ->
            receive
                ["I"|List] ->
                    ?LOG({"parser:start received init message", List}),
                    %% I dx dy time-limit min-sensor max-sensor max-speed max-turn max-hard-turn ;
                    [Width,Height,TimeLimit,MinSensor,MaxSensor,MaxSpeed,MaxTurn,MaxHardTurn] =
                        List,

                    World = #world{
                          width=str2num(Width),
                          height=str2num(Height),
                          timelimit=str2num(TimeLimit),
                          minsensor=str2num(MinSensor),
                          maxsensor=str2num(MaxSensor),
                          maxspeed=str2num(MaxSpeed),
                          maxturn=str2num(MaxTurn),
                          maxhardturn=str2num(MaxHardTurn)
                         },
                    Main ! {world, World},

                    ?LOG({"parser entering main loop"}),
                    loop(Controller, Steer, Map, Pathfind)
            end
    end.


loop(Controller, Steer, Map, Pathfind) ->
    receive
        ["T"|List] ->
            %% ?LOG({"parser:loop T message received",List}),
            parse_map(Map,
                      parse_rover(Steer, Pathfind, List));
        ["B", _] ->
            Controller ! {bump, boulder};
        ["C", _] ->
            Controller ! {reset, crater};
        ["K", _] ->
            Controller ! {reset, killed};
        ["E", _, _] ->
            Controller ! {reset, endofround};
        ["S", _] ->
            ?LOG({"parser:loop received score, ignoring"});
        Any ->
            ?LOG({"parser:loop received unknown message", Any})
    end,
    loop(Controller, Steer, Map, Pathfind).




parse_rover(Steer, Pathfind, List) ->
    %% T time-stamp vehicle-ctl vehicle-x vehicle-y vehicle-dir vehicle-speed objects ;
    [_,VehicleCtl,VehicleX,VehicleY,VehicleDir,VehicleSpeed|ObjectList] = List,
    X = str2num(VehicleX),
    Y = str2num(VehicleY),

    Rover = #rover{
      turn=string:substr(VehicleCtl, 2),
      accel=string:left(VehicleCtl, 1),
      x=X,
      y=Y,
      dir=(sanitize_dir(str2num(VehicleDir))),
      speed=str2num(VehicleSpeed)
     },

    Steer ! {rover, Rover},
    Pathfind ! {pos, {X, Y}},

    ObjectList.



parse_map(_, []) ->
    ok;

parse_map(Map, [Type, X1, Y1, Dir1, Speed1|Rest]) when Type =:= "m" ->
    X = str2num(X1),
    Y = str2num(Y1),
    Dir = str2num(Dir1),
    Speed = str2num(Speed1),

    Map ! {martian, {X, Y, Dir, Speed}},

    parse_map(Map, Rest);

parse_map(Map, [Type, X1, Y1, R1|Rest]) ->
    X = str2num(X1),
    Y = str2num(Y1),
    R = str2num(R1),
    case Type of
        "b" ->
            Map ! {boulder,{X, Y, R}};
        "c" ->
            Map ! {crater,{X, Y, R}};
        "h" ->
            %% ?LOG({"parser:parse_map seeing home, ignoring",{X, Y, R}});
            ok;
        Any ->
            throw({"parser:parse_map garbage object type", Any})
    end,
    parse_map(Map, Rest);

parse_map(_, [Any]) ->
    throw({"parser:parse_map garbage at end of message", Any}).





str2num(Str) ->
    {Num, _} = string:to_float(Str),
    Num.

sanitize_dir(Dir) ->
    if
        Dir < 0 -> ((Dir+360)/180)*math:pi();
        Dir >= 360 -> ((Dir-360)/180)*math:pi();
        true -> (Dir/180)*math:pi()
    end.
