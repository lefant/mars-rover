-module(parser).
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



%%%%% telemetry message parsing

test() ->
    
    ok.



parse_init_message(World,["I"|List]) ->
    %% I dx dy time-limit min-sensor max-sensor max-speed max-turn max-hard-turn ;
    [Width,Height,TimeLimit,MinSensor,MaxSensor,MaxSpeed,MaxTurn,MaxHardTurn,_] = List,

    QuadTree = quadtree:new(trunc(str2num(Width)/2)),

    World#world{
      width=str2num(Width),
      height=str2num(Height),
      timelimit=str2num(TimeLimit),
      minsensor=str2num(MinSensor),
      maxsensor=str2num(MaxSensor),
      maxspeed=str2num(MaxSpeed),
      maxturn=str2num(MaxTurn),
      maxhardturn=str2num(MaxHardTurn),
      quadtree=QuadTree,
      curnode=QuadTree
     }.

parse_message(World,["T"|List]) ->
    %% T time-stamp vehicle-ctl vehicle-x vehicle-y vehicle-dir vehicle-speed objects ;
    [_,VehicleCtl,VehicleX,VehicleY,VehicleDir,VehicleSpeed|ObjectList] = List,
    X = str2num(VehicleX),
    Y = str2num(VehicleY),
    CurNode = quadtree:find_node(World#world.quadtree,{X,Y}),
    E = quadtree:eq_node(CurNode,World#world.curnode),
    if
        not E ->
            {SubGoal,Path} = quadtree:next_subgoal(World#world.path),
            ok;
        true ->
            SubGoal = World#world.goal,
            Path = World#world.path
    end,

    World1 = World#world{
               turn=string:substr(VehicleCtl, 2),
               accel=string:left(VehicleCtl, 1),
               x=X,
               y=Y,
               dir=(sanitize_dir(str2num(VehicleDir))),
               speed=str2num(VehicleSpeed),
               curnode=CurNode,
               goal=SubGoal,
               path=Path
              },
    parse_object_list(World1,ObjectList);

parse_message(World,["E",Time,Score]) ->
    io:format("EVENT: end of round: time: ~p score: ~p~n",[Time,Score]),
    visualizer ! {clear},
    quadtree:visualize(World#world.quadtree,white),
    Path = quadtree:astar(
             World#world.quadtree,
             {World#world.x,World#world.y},
             {0,0}),
    World1 = World#world{
               path=Path
              },
    World1;
parse_message(World,["S",Score]) ->
    io:format("EVENT: end of round: score: ~p~n",[Score]),
    World;
parse_message(World,["B",Time]) ->
    io:format("EVENT: Boulder crash: ~p~n",[Time]),
    World;
parse_message(World,["C",Time]) ->
    io:format("EVENT: Crater crash: ~p~n",[Time]),
    World;
parse_message(World,["K",Time]) ->
    io:format("EVENT: Killed by Martian!: ~p~n",[Time]),
    World;
parse_message(World,[Event,Time]) ->
    io:format("EVENT: unknown!!! time: ~p kind of event: ~p~n",[Time,Event]),
    World;
parse_message(World,Msg) ->
    io:format("unknown message: ~p~n",[Msg]),
    throw({unknown_msg,World,Msg}).


sanitize_dir(Dir) ->
    if
        Dir < 0 -> ((Dir+360)/180)*math:pi();
        Dir >= 360 -> ((Dir-360)/180)*math:pi();
        true -> (Dir/180)*math:pi()
    end.

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
                    World1 = update_quadtree(World,{X,Y,R}),
                    parse_object_list(
                      World1#world{
                        boulders=[{X,Y,R}|World1#world.boulders]
                       },Rest)
            end;
        "c" ->
            AlreadyKnown = lists:member(
                             {X,Y,R},
                             World#world.craters),
            if
                AlreadyKnown -> parse_object_list(World,Rest);
                true ->
                    World1 = update_quadtree(World,{X,Y,R}),
                    parse_object_list(
                      World1#world{
                        craters=[{X,Y,R}|World1#world.craters]
                       },Rest)
            end;
        "h" ->
            %%parse_object_list(World#world{home={X,Y,R}},Rest)
            parse_object_list(World,Rest);
        Crap ->
            io:format("parse_object_list: received inner crap ~p~n",[Crap]),
            World
    end;

parse_object_list(World,[Crap]) ->
    io:format("parse_object_list: received outer crap ~p~n",[Crap]),
    World.



update_quadtree(World,Circle) -> 
    QuadTree =
        quadtree:insert_circle(
          World#world.quadtree,
          Circle),
    Path = quadtree:astar(
             QuadTree,
             {World#world.x,World#world.y},
             {0,0}),
    World1 = World#world{
               quadtree=QuadTree,
               path=Path
              },
    World1.



str2num(Str) ->
    {Num,_} = string:to_float(Str),
    Num.
