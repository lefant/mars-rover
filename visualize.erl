-module(visualize).
-compile(export_all).
%%-export([bench/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").


test() ->
    ?LOG("Debug is enabled"),

    {A,B,C} = erlang:now(),
    random:seed(A,B,C),

    start(),
    %% timer:sleep(100),

    visualizer ! {oval, {40,0}, orange},

    lists:map(
      fun(_) ->
              %% ?LOG("sending random point to visualizer"),
              visualizer ! {oval, random_point(), orange}
      end,
      lists:seq(1,50)),

    ok.


random_point() ->
    {random:uniform(400)-200,random:uniform(400)-200}.




start() ->
    Pid = spawn(
            fun() ->
                    % initialization
                    I=gs:start(),
                    Win=gs:create(window, I,
                                  [{width, 1000},{height, 1000},
                                   {title,"quadtree visualization"},{map, true}]),
                    gs:create(canvas, can1,Win,
                              [{x,0},{y, 0},{width,1000},{height,1000}]),

                    % main loop
                    loop()
            end),
    register(visualizer, Pid),
    ?LOG("loop spawned and registered"),
    ok.

                    


loop() ->
    receive
        {clear} ->
            gs:create(rectangle,can1,
                      [{coords,
                        [transform({0,0}),
                         transform({0,0})]},
                       {fill,grey}]);
        {oval,Point,Color} ->
            %% ?LOG({"visualize loop: oval",Point,Color}),
            draw_oval(Point,Color,3),
            loop();
        {oval,Point,Color,R} ->
            %% ?LOG({"visualize loop: oval",Point,Color}),
            draw_oval(Point,Color,R),
            loop();
        {node,Points,Color} ->
            %% ?LOG({"visualize loop: node",Points,Color}),
            draw_rect(Points,Color),
            loop();
        {gs,_,destroy,[],[]} ->
            ?LOG({"visualize loop: window closed, exiting"});
        Any ->
            ?LOG({"visualize loop: unknown msg: ", Any}),
            loop()
    end.




%%%%% drawing helper functions

draw_rect({{X1,Y1},{X2,Y2}},Color) ->
    gs:create(rectangle,can1,
              [{coords,
                [transform({X1,Y1}),
                 transform({X2,Y2})]},
               {fill,Color}]).

draw_oval({X,Y},Color,R) ->
    gs:create(oval,can1,
              [{coords,
                [transform({X-R,Y-R}),
                 transform({X+R,Y+R})]},
               {fill,Color}]),
    ok.


transform({X,Y}) ->
    {(X+200),
     (-Y+200)}.
