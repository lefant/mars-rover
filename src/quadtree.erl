-module(quadtree).
-export([test/0,new/1,insert_circle/2,next_subgoal/1,astar/3,find_node/2,eq_node/2]).

-include("../include/debug.hrl").
-include("../include/quadtree.hrl").

%% -define(MINSIZE, 5).


test() ->
    test_intersects(),

    visualize:start(),

    {Tree, Start, Goal} = gen_testquad(),

    %% visualize(Tree,white),

    {X,Y} = Goal,
    Path = astar(Tree, Start, {X, Y, 5}),

    if
        Path =:= failure ->
            ?LOG({"test: NO PATH found"}),
            ok;
        true ->
            ?LOG({"test: PATH found"}),
            visualize(Path, yellow),
            _ = next_subgoal(Path)
    end,

    visualizer ! {oval, Start, blue},
    visualizer ! {oval, Goal, orange},

    HomeNodes = home_nodes(Tree, {0, 0, 5}),
    visualize(HomeNodes, green),

    ok.


gen_testquad() ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),

    QuadTree = new(200),

    Start = random_point(),
    Goal = random_point(),


    ListOfCircles =
        lists:filter(
          fun({X, Y, R})
             -> not within_circle({X, Y, R+5}, Goal)
          end,
          lists:filter(
            fun({X, Y, R})
               -> not within_circle({X, Y, R+5}, Start)
            end,
            lists:map(
              fun(_) ->
                      {X, Y} = random_point(),
                      {X, Y, random:uniform(30)}
              end,
              string:left("", 20)))),


    Tree = lists:foldl(
        fun(Circle, Node)
           -> insert_circle(Node, Circle)
        end,
        QuadTree,
        ListOfCircles),

    {Tree, Start, Goal}.

random_point() ->
    {random:uniform(400)-200, random:uniform(400)-200}.





new(Size) ->
    #quadtree{
     x=0,
     y=0,
     size=Size
    }.






next_subgoal([]) ->
    throw({empty_path,"cannot compute next subgoal of an empty path"});
next_subgoal(no_path) ->
    throw({no_path,"cannot compute next subgoal when there is no path"});
next_subgoal([_]) ->
    ?LOG({"next_subgoal, final subgoal: ",{0, 0}}),
    {{0, 0}, no_next_node, []};
next_subgoal([LastNode, CurNode]) ->
    next_subgoal([LastNode, CurNode, #quadtree{x=0, y=0, size=5}]);
next_subgoal([CurNode, NextNode, NexterNode|Path]) ->
    %% N = {NextNode#quadtree.x, NextNode#quadtree.y},

    NextGoal = midpoint(
        nodes_touching(CurNode, NextNode),
        nodes_touching(NextNode, NexterNode)),

    visualize(NextNode, yellow),
    {NextGoal, NextNode, [NextNode|[NexterNode|Path]]}.



astar(Tree, StartPoint, Home) ->
    {X, Y, _} = Home,
    GoalPoint = {X, Y},
    StartNode = find_node(Tree, StartPoint),

    %% GoalNode = find_node(Tree, GoalPoint),
    GoalNodes = home_nodes(Tree, Home),
    [GoalNode|_] = GoalNodes,

    E = eq_node(StartNode, GoalNode),
    if
        E -> [StartNode];
        true -> astar(Tree, GoalPoint, GoalNode, [], [{StartNode, 0, []}])
    end.
astar(_,_,_,_,[]) ->
    failure;
astar(Tree, GoalPoint, GoalNode, Closed, [{Node, CostSoFar, PathSoFar}|Open]) ->
    IsGoalReached = eq_node(Node, GoalNode),
    if
        IsGoalReached ->
            lists:reverse([Node|PathSoFar]);
        true ->
            OpenNeighbours =
                lists:filter(
                  fun(MaybeOpenNode) ->
                          not lists:any(
                                fun(ClosedNode) ->
                                        eq_node(MaybeOpenNode, ClosedNode)
                                end,
                                Closed)
                  end,
                  neighbours(Tree, Node)),

            NewOpen =
                lists:foldl(
                  fun(NeighbourNode, Open1) ->
                          NeighbourDist = node_dist2(Node, NeighbourNode),
                          AlreadyOpen =
                              lists:filter(
                                fun({OpenNode, _, _}) ->
                                        eq_node(NeighbourNode, OpenNode)
                                end,
                                Open1),
                          case AlreadyOpen of
                              [] ->
                                  [{NeighbourNode,
                                    CostSoFar+NeighbourDist,
                                    [Node|PathSoFar]}|Open1];
                              [OldNode] ->
                                  {OpenNode, OldCost, _} = OldNode,
                                  NewCost = CostSoFar+NeighbourDist,
                                  if
                                      NewCost < OldCost ->
                                          replace_node(
                                            {OpenNode, NewCost, [Node|PathSoFar]},
                                            Open1);
                                       true ->
                                          Open1
                                  end
                          end
                  end,
                  Open,
                  OpenNeighbours),

            SortedOpen =
                lists:sort(
                  fun({Node1, _, _}, {Node2, _, _}) ->
                          min_dist2(Node1, GoalPoint) < min_dist2(Node2, GoalPoint)
                  end,
                  NewOpen),

            astar(Tree, GoalPoint, GoalNode, [Node|Closed], SortedOpen)
    end.
            








insert_circle(Node, Circle) ->
    I = intersects_circle(Node, Circle),
    if
        I ->
            if
                is_list( Node#quadtree.children ) ->
                    Node#quadtree{
                      children=lists:map(
                                 fun(ChildNode) ->
                                         insert_circle(ChildNode, Circle)
                                 end,
                                 Node#quadtree.children)};
                true ->
                    N = node_within_circle(Node, Circle),
                    O = Node#quadtree.status=:=obstacle,
                    if
                        (((Node#quadtree.size > ?MINSIZE)
                        and not N) and not O) ->
                            Node#quadtree{
                              children=lists:map(
                                         fun(ChildNode) ->
                                                 visualize(ChildNode, white),
                                                 insert_circle(ChildNode, Circle)
                                         end,
                                         new_children(Node)),
                              status=parent};
                        true ->
                            visualize(Node, red),
                            Node#quadtree{status=obstacle}
                    end
            end;
        true ->
            Node
    end.


find_node(Node, {X, Y}) ->
    %% io:format("find_node: ~p ~p~n", [string:copies(" ",50-trunc(2*Node#quadtree.size)),{Node#quadtree.x,Node#quadtree.y}]),
    if
        is_list(Node#quadtree.children) ->
            %% ?LOG({"find_node: children"}),
            [MyChild] = lists:filter(
              fun(ChildNode) ->
                      within_node(ChildNode, {X, Y})
              end,
              Node#quadtree.children),
            find_node(MyChild, {X, Y});
        true -> 
            %% ?LOG({"find_node: leafnode"}),
            Node
    end.

home_nodes(Node, Circle) ->
    if
        is_list(Node#quadtree.children) ->
            lists:flatmap(
              fun(ChildNode) ->
                      home_nodes(ChildNode, Circle)
              end,
              Node#quadtree.children);
        true ->
            I = intersects_circle(Node, Circle),
            if
                I -> [Node];
                true -> []
            end
    end.





neighbours(Node, Leaf) ->
    lists:filter(
      %% fun(ChildNode) -> not eq_node(Leaf,ChildNode) end,
      fun(ChildNode) ->
              Res = not lists:all(
                    fun(Point) -> within_node(Leaf, Point) end,
                    corners(
                      {ChildNode#quadtree.x,
                       ChildNode#quadtree.y,
                       ChildNode#quadtree.size - ?MINSIZE/2})),
              %% ?LOG({"neighbours: within",Res}),
              Res
      end,
      neighbours_rec(Node,
                     Leaf#quadtree{
                       x = Leaf#quadtree.x + ?MINSIZE/2,
                       size = Leaf#quadtree.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#quadtree{
                       x = Leaf#quadtree.x - ?MINSIZE/2,
                       size = Leaf#quadtree.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#quadtree{
                       y = Leaf#quadtree.y + ?MINSIZE/2,
                       size = Leaf#quadtree.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#quadtree{
                       y = Leaf#quadtree.y - ?MINSIZE/2,
                       size = Leaf#quadtree.size - ?MINSIZE/4
                      })).


neighbours_rec(Node, Leaf) ->
    I = intersects_node(Node, Leaf),
    if
        I ->
            %% ?LOG({"neighbours: intersects"}),
            if
                is_list(Node#quadtree.children) ->
                    %% ?LOG({"neighbours: children"}),
                    lists:flatmap(
                      fun(ChildNode) ->
                              neighbours_rec(ChildNode, Leaf)
                      end,
                      lists:filter(
                        fun(ChildNode) ->
                                intersects_node(ChildNode, Leaf)
                        end,
                        Node#quadtree.children));
                true ->
                    %% ?LOG({"neighbours: leafnode"}),
                    if
                        Node#quadtree.status =:= empty -> [Node];
                        true -> []
                    end
            end;
        true -> []
    end.

new_children(Node) ->
    NewSize = Node#quadtree.size/2,
    [
     #quadtree{
      x=Node#quadtree.x+NewSize,
      y=Node#quadtree.y+NewSize,
      size=NewSize
     },
     #quadtree{
      x=Node#quadtree.x+NewSize,
      y=Node#quadtree.y-NewSize,
      size=NewSize
     },
                #quadtree{
      x=Node#quadtree.x-NewSize,
      y=Node#quadtree.y+NewSize,
      size=NewSize
     },
                #quadtree{
      x=Node#quadtree.x-NewSize,
      y=Node#quadtree.y-NewSize,
      size=NewSize
     }].







%%%%% graphics for demo / debugging purposes

% visualize/2 takes a Node or list of Nodes and a color
% will then recurse if necessary and draw it all
visualize([], _) ->
    ok;
visualize([Node|List], Color) ->
    visualize(Node, Color),
    visualize(List, Color);
visualize(Node, Color) ->
    if
        is_list(Node#quadtree.children) ->
            lists:map(
              fun(ChildNode) ->
                      visualize(ChildNode, Color)
              end,
              Node#quadtree.children);
        true ->
            [X1,X2,_,_] = node_corners(Node),
            case Node#quadtree.status of
                obstacle -> Color1 = red;
                empty -> Color1 = Color
            end,
            visualizer ! {node, {X1, X2}, Color1}
    end.





%%%%% node and geometrical helper functions

replace_node(Item, List) ->
    replace_node(Item, List, []).
replace_node(_, [], Res) ->
    Res;
replace_node(Item, [H|List], Res) ->
    {NewNode, _, _} = Item,
    {OldNode, _, _} = H,
    E = eq_node(NewNode, OldNode),
    if
        E -> replace_node(Item, List, [Item|Res]);
        true -> replace_node(Item, List, [H|Res])
    end.


eq_node(#quadtree{x=X, y=Y}, #quadtree{x=X, y=Y}) ->
    true;
eq_node(_, _) ->
    false.



within_node(Node, Point) ->
    within(Point, node_corners(Node)).

intersects_node(Node1, Node2) ->
    C1 = node_corners(Node1),
    C2 = node_corners(Node2),
    lists:any(
      fun(Point) -> within(Point, C1) end,
      C2) or
        lists:any(
          fun(Point) -> within(Point, C2) end,
          C1).

node_within_circle(Node, Circle) ->
    lists:all(
      fun(Point) -> within_circle(Circle, Point) end,
      node_corners(Node)).


nodes_touching(N1, N2) ->
    C1 = node_corners(N1),
    C2 = node_corners(N2),

    ListA = lists:filter(
              fun(Point) ->
                      within_closed(Point, C1)
              end,
              C2),
    ListB = lists:filter(
              fun(Point) ->
                      within_closed(Point, C2) end,
              C1),

    CommonPointList = lists:usort(
      fun({X1, Y1}, {X2, Y2}) ->
              (X1 =< X2) and (Y1 =< Y2)
      end,
      ListA ++ ListB),

    case CommonPointList of
        [] ->
            ?LOG({"nodes_touching: hmpf! NOT touching", N1, N2}),
            {N2#quadtree.x, N2#quadtree.y};
        %% [P] ->
        %%     visualizer ! {oval, P, black},
        %%     NextGoal = midpoint(N,P);
        [P1, P2, P3, P4] ->
            ?LOG({"nodes_touching: hmpf! 4 touching", N1, N2}),
            visualizer ! {oval, P1, yellow},
            visualizer ! {oval, P2, yellow},
            visualizer ! {oval, P3, yellow},
            visualizer ! {oval, P4, yellow},
            midpoint(midpoint(P1, P2), midpoint(P3, P4));
        [P1, P2] ->
            visualizer ! {oval, P1, black},
            visualizer ! {oval, P2, black},
            midpoint(P1, P2)
    end.


within({X, Y}, [{X1, Y1}, {X2, Y2}, _, _]) ->
    ((X1 < X) and (X =< X2))
        and
          ((Y1 < Y) and (Y =< Y2)).

within_closed({X,Y}, [{X1, Y1}, {X2, Y2}, _, _]) ->
    ((X1 =< X) and (X =< X2))
        and
          ((Y1 =< Y) and (Y =< Y2)).

node_corners(Node) ->
    corners({Node#quadtree.x,
             Node#quadtree.y,
             Node#quadtree.size}).

corners({X, Y, Size}) ->
    [{X-Size, Y-Size},
     {X+Size, Y+Size},
     {X+Size, Y-Size},
     {X-Size, Y+Size}].


within_circle({X, Y, R}, Point) ->
    dist2({X, Y}, Point) =< sqr(R).


min_dist2(Node, Point) ->
    dist2(
      {Node#quadtree.x, Node#quadtree.y},
      Point).
    %% lists:min(
    %%   lists:map(
    %%     fun(NPoint) -> dist2(Point,NPoint) end,
    %%     node_corners(Node))).

node_dist2(Node1, Node2) ->
    dist2({Node1#quadtree.x, Node1#quadtree.y},
          {Node2#quadtree.x, Node2#quadtree.y}).

dist2({X1, Y1}, {X2, Y2}) ->
    sqr(X1-X2) + sqr(Y1-Y2).

midpoint({X1, Y1}, {X2, Y2}) ->
    {(X1+X2)/2, (Y1+Y2)/2}.

sqr(X) ->
    X*X.













intersects_circle(Node, {X, Y, R}) ->
    NodeXmax = Node#quadtree.x+Node#quadtree.size,
    NodeXmin = Node#quadtree.x-Node#quadtree.size,
    NodeYmax = Node#quadtree.y+Node#quadtree.size,
    NodeYmin = Node#quadtree.y-Node#quadtree.size,

    %% leftmost point of circle left of right vertical boundary
    Er = (X-R =< NodeXmax),
    %% rightmost point of circle right of left vertical boundary
    Wr = (NodeXmin =< X+R),
    CloseHorizontally = Er and Wr,

    %% lowermost point of circle below of upper horizontal boundary
    Nr = (Y-R =< NodeYmax),
    %% uppermost point of circle above of lower horizontal boundary
    Sr = (NodeYmin =< Y+R),
    CloseVertically = Nr and Sr,

    %% center of circle left of right vertical boundary
    Ec = (X =< NodeXmax),
    %% center of circle right of left vertical boundary
    Wc = (NodeXmin =< X),
    WithinHorizontally = Ec and Wc,

    %% center of circle below of upper horizontal boundary
    Nc = (Y =< NodeYmax),
    %% center of circle above of lower horizontal boundary
    Sc = (NodeYmin =< Y),
    WithinVertically = Nc and Sc,

    if
        WithinHorizontally and WithinVertically -> true;
        WithinHorizontally and CloseVertically -> true;
        WithinVertically and CloseHorizontally -> true;
        CloseVertically and CloseHorizontally ->
            R2 = sqr(R),
            XDistMax = sqr(X-NodeXmax),
            XDistMin = sqr(X-NodeXmin),
            YDistMax = sqr(Y-NodeYmax),
            YDistMin = sqr(Y-NodeYmin),
            if
                XDistMax+YDistMax < R2 -> true;
                XDistMax+YDistMin < R2 -> true;
                XDistMin+YDistMax < R2 -> true;
                XDistMin+YDistMin < R2 -> true;
                true -> false
            end;
        true -> false
    end.


test_intersects() ->
    QuadTree = #quadtree{
      x=0,
      y=0,
      size=20
     },

    false = intersects_circle(QuadTree,{100,100,1}),
    false = intersects_circle(QuadTree,{-100,-100,1}),
    false = intersects_circle(QuadTree,{100,-100,1}),
    false = intersects_circle(QuadTree,{-100,100,1}),

    false = intersects_circle(QuadTree,{100,100,85}),
    false = intersects_circle(QuadTree,{-100,100,85}),
    false = intersects_circle(QuadTree,{100,-100,85}),
    false = intersects_circle(QuadTree,{-100,-100,85}),

    false = intersects_circle(QuadTree,{100,0,1}),
    false = intersects_circle(QuadTree,{0,100,1}),
    false = intersects_circle(QuadTree,{-100,0,1}),
    false = intersects_circle(QuadTree,{0,-100,1}),

    true = intersects_circle(QuadTree,{0,0,10}),

    true = intersects_circle(QuadTree,{25,0,10}),
    true = intersects_circle(QuadTree,{-25,0,10}),
    true = intersects_circle(QuadTree,{0,25,10}),
    true = intersects_circle(QuadTree,{0,-25,10}),

    true = intersects_circle(QuadTree,{25,25,10}),
    true = intersects_circle(QuadTree,{-25,25,10}),
    true = intersects_circle(QuadTree,{25,-25,10}),
    true = intersects_circle(QuadTree,{-25,-25,10}),

    true = intersects_node(
             #quadtree{x=0,y=0,size=10},
             #quadtree{x=19,y=19,size=10}),
    ok.






%% find_parent(Node,Leaf) ->
%%     if
%%         is_list(Node#quadtree.children) ->
%%             [MyChild] = lists:filter(
%%               fun(ChildNode) ->
%%                       within_node(ChildNode,{Leaf#quadtree.x,Leaf#quadtree.y})
%%               end,
%%               Node#quadtree.children),
%%             if
%%                 is_list(MyChild#quadtree.children) ->
%%                     find_parent(MyChild,Leaf);
%%                 true -> Node
%%             end;
%%         true -> throw(empty_tree)
%%     end.

