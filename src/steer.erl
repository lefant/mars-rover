-module(steer).
-export([start/0,test/0]).

-include("../include/debug.hrl").
-include("../include/rover.hrl").
-include("../include/map.hrl").


test() ->
    test_pov_funcs(),
    ok.


start() ->
    receive
        {start, {Controller, Pathfind, MaxSpeed}} ->
            ?LOG({"steer started, waiting for initial map message"}),
            receive
                {map, Map} ->
                    ?LOG({"steer started, waiting for initial rover message"}),
                    receive
                        {rover, Rover} ->
                            ?LOG({"steer waiting for initial goal"}),
                            receive
                                {goal, Goal} ->
                                    visualizer ! {oval, Goal, green},
                                    maybe_command(Controller, Rover, Goal, Map, MaxSpeed),
                                    ?LOG({"steer entering main loop"}),
                                    loop(Controller, Pathfind, Rover, Goal, Map, MaxSpeed)
                            end
                    end
            end
    end.

loop(Controller, Pathfind, Rover, Goal, Map, MaxSpeed) ->
    receive
        {rover, Rover1} ->
            maybe_command(Controller, Rover1, Goal, Map, MaxSpeed),
            loop(Controller, Pathfind, Rover1, Goal, Map, MaxSpeed);
        {goal, Goal1} ->
            visualizer ! {oval, Goal, yellow},
            visualizer ! {oval, Goal1, green},
            maybe_command(Controller, Rover, Goal1, Map, MaxSpeed),
            loop(Controller, Pathfind, Rover, Goal1, Map, MaxSpeed);
        {reset} ->
            receive
                {rover, Rover1} ->
                    ?LOG({"steer waiting for initial goal"}),
                    receive
                        {goal, Goal1} ->
                            visualizer ! {oval, Goal1, green},
                            maybe_command(Controller, Rover1, Goal1, Map, MaxSpeed),
                            loop(Controller, Pathfind, Rover1, Goal1, Map, MaxSpeed)
                    after 500 ->
                            Goal1 = {0, 0},
                            visualizer ! {oval, Goal1, green},
                            maybe_command(Controller, Rover1, Goal1, Map, MaxSpeed),
                            loop(Controller, Pathfind, Rover1, Goal1, Map, MaxSpeed)
                    end
            end;
        {map, Map1} ->
            %% ?LOG({"steer loop: map update", Map1}),
            loop(Controller, Pathfind, Rover, Goal, Map1, MaxSpeed);
        Any ->
            ?LOG({"steer loop: unknown msg", Any}),
            loop(Controller, Pathfind, Rover, Goal, Map, MaxSpeed)
    end.





maybe_command(Controller, Rover, Target, Map, MaxSpeed) ->
    Steering = seek(Rover, Target, MaxSpeed),
    RoverSteering = rotate(-Rover#rover.dir, Steering),

    %% ?LOG({"steer:maybe_command roversteering:", RoverSteering}),

    SeekTurn = turn_goal( v2turn(RoverSteering) ),
    SeekAccel = accel_goal( RoverSteering ),

    {TurnAfterEmergencies, AccelAfterEmergencies} =
        correct_for_emergency(Rover, Map, {SeekTurn, SeekAccel}),

    TurnCommand = correct_turn(Rover#rover.turn, TurnAfterEmergencies),
    AccelCommand = correct_accel(Rover#rover.accel, AccelAfterEmergencies),

    %% SeekSteering = v_add(Rover#rover.pos, Steering),
    %% visualizer ! {oval, SeekSteering, green, 2},
    %% visualizer ! {oval, future_pos(Rover), orange, 2},
    visualizer ! {oval, Rover#rover.pos, blue, 2},

    if
        (AccelCommand =:= "") and (TurnCommand =:= "") -> void;
        true ->
            Command = list_to_binary(string:join([AccelCommand, TurnCommand, ";"], "")),

            %% ?LOG({"steer steering: ", Rover#rover.timestamp,
            %%       {AccelCommand, TurnCommand},
            %%       {Rover#rover.speed}}),

            Controller ! {command, Command}
    end.


correct_for_emergency(Rover, Map, SeekSteering) ->
    %% %% where will the rover be in 1 second?
    %% FuturePos = v_add(Rover#rover.pos, velocity(Rover)),

    %% build a list of all dangers, excluding ones
    %% we cannot hit without going at least 100
    Obstacles = Map#map.craters ++ Map#map.boulders,

    SomewhatCloseObstacles =
        lists:filter(
          fun({X,Y,R})->
                  lists:min([abs(X-Rover#rover.x),abs(Y-Rover#rover.y)]) - R < 50
          end,
          Obstacles),

    %% convert all coordinates to ones relative to our vehicle facing
    %% forward on the Y-axis
    ObstaclesRelative =
        lists:map(
          fun({X,Y,R})->
                  {X1,Y1} = to_rover_coordinates(Rover, {X, Y}),
                  {X1,Y1,R}
          end,
          SomewhatCloseObstacles),

    ObstaclesUpfront =
        lists:filter(
          fun({X,Y,R})->
                  ((X > 0) and (X-R < Rover#rover.speed)) and (abs(Y) < R+1)
          end,
          ObstaclesRelative),

    SortedObstacles = lists:keysort(1,ObstaclesUpfront),

    case SortedObstacles of
        [] -> SeekSteering;
        [{_X, Y, _R}=ClosestObstacle|_] ->
            ?LOG({"emergency corrective steering required: ",ClosestObstacle}),
            if
                Y >= 0 -> {"R", "b"};
                Y < 0 -> {"L", "b"}
            end
    end.



%%% seek behaviour according to http://www.red3d.com/cwr/steer/gdc99/
%%
%% desired_velocity = normalize (position - target) * max_speed
%% steering = desired_velocity - velocity
seek(Rover, Target, MaxSpeed) ->
    %% vector pointing from current position to target
    RoverTarget = v_sub(
                    Target,
                    Rover#rover.pos),

    %% heading with max speed towards target
    DesiredVelocity = v_mul( MaxSpeed, normalize( RoverTarget ) ),
    Steering = v_sub( DesiredVelocity, velocity(Rover) ),
    Steering.


velocity(Rover) ->
    Sin = math:sin(Rover#rover.dir),
    Cos = math:cos(Rover#rover.dir),
    Speed = Rover#rover.speed,
    Vx = Cos * Speed,
    Vy = Sin * Speed,
    {Vx, Vy}.

normalize({X, Y}) ->
    Length = v_len({X, Y}),
    {X/Length, Y/Length}.


%% transforms vector coordinates so that
%% (1,0) will be 1m away directly in front of the rover and
%% (0,1) will be exactly 1m to the left of the rover
to_rover_coordinates(Rover, X) ->
    coordinate_transform(
      Rover#rover.pos,
      -Rover#rover.dir,
      X).

coordinate_transform(NewOrigin, Dir, X) ->
    X1 = v_sub(X, NewOrigin),
    X2 = rotate(Dir, X1),
    X2.

%% rotates vector by Dir
rotate(Dir, {X, Y}) ->
    Sin = math:sin(Dir),
    Cos = math:cos(Dir),
    X1 = X*Cos-Y*Sin,
    Y1 = Y*Cos+X*Sin,
    {X1, Y1}.

%% simple vector algebra helpers
v_add({X1, Y1}, {X2, Y2}) ->
    {X1+X2, Y1+Y2}.
v_sub({X1, Y1}, {X2, Y2}) ->
    {X1-X2, Y1-Y2}.
v_mul(C, {X, Y}) ->
    {C * X, C * Y}.
v_len({X, Y}) ->
    math:sqrt(X*X) + math:sqrt(Y*Y).





v2turn({Vx, Vy}) ->
    if
        Vx < 0 ->
            if
                Vy =:= 0 -> Vy1 = 100;
                true -> Vy1 = Vy*100
            end;
        true ->
            Vy1 = Vy
    end,
    %% ?LOG({"steer:v2turn ", {Vx, Vy}, Vy1}),
    Vy1.

turn_goal(Diff) ->
    HardTresh = 12,
    StraightTresh = 3,
    if
        Diff < (-1*HardTresh) -> "R";
        ((-1*HardTresh) =< Diff) and (Diff < StraightTresh)-> "r";
        ((-1*StraightTresh) =< Diff) and (Diff =< StraightTresh) -> "-";
        (StraightTresh < Diff) and (Diff < HardTresh)-> "l";
        HardTresh < Diff -> "L"
    end.


accel_goal({Vx, _Vy}) ->
    D = abs(Vx),
    if
        D < 0.1 -> "-";
        Vx > 0 -> "a";
        Vx < 0 -> "b"
    end.









%% mini do-it yourself state machine style correcting current
%% state of controls to the desired one

correct_turn(Cur,Goal) ->
    case Goal of
        Cur -> "";
        "R" -> "r";
        "L" -> "l";
        "r" ->
            if
                Cur =:= "R" -> "l";
                true -> "r"
            end;
        "l" ->
            if
                Cur =:= "L" -> "r";
                true -> "l"
            end;
        "-" ->
            case Cur of
                "L" -> "r";
                "l" -> "r";
                "R" -> "l";
                "r" -> "l"
            end
    end.

correct_accel(Cur, Goal) ->
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
test_pov(X, Y, E) ->
    {X1, Y1} = coords_to_pov(0, 0, E*(math:pi()/2), {X, Y}),
    {trunc(X1), trunc(Y1)}.

coords_to_pov(Ox, Oy, Dir, {X, Y}) ->
    coordinate_transform({Ox, Oy}, Dir, {X, Y}).
