-module(quadtree).
-compile(export_all).
%%-export([bench/0]).


%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.
-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).

-define(MINSIZE, 0.5).

-record(node, {
          x,
          y,
          size,
          children=undefined,
          status=empty
}).

test() ->
    ?LOG("Debug is enabled"),
    dbg:tracer(),
    dbg:p(all, call),
%%%     dbg:tpl(quadtree, astar, 5, []),
%%%     dbg:tpl(quadtree, within_circle, 2, []),
%%%     dbg:tpl(quadtree, within_node, 2, []),
%%%     dbg:tpl(quadtree, intersects_node, 2, []),
%%%     dbg:tpl(quadtree, within, 2, []),
%%%     dbg:tpl(quadtree, corners, 1, []),
%%%     dbg:tpl(quadtree, intersects_circle, 2, []),

    {A,B,C} = erlang:now(),
    random:seed(A,B,C),

    %% ListOfCircles = [
    %%                  {-10,-5,3},
    %%                  {3,-4,2},
    %%                  {17,13,5}
    %%                  ],
    ListOfCircles =
        lists:map(
          fun(_) ->
                  {random:uniform(40)-20,random:uniform(40)-20,random:uniform(5)}
          end,
          [1,1,1,1,1]),

    QuadTree = #node{
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
             #node{x=0,y=0,size=10},
             #node{x=19,y=19,size=10}),

    Tree = lists:foldl(
        fun(Circle,Node)
           -> insert_circle(Node,Circle)
        end,
        QuadTree,
        ListOfCircles),

%%%     walk_tree(Tree),
    MyNode = find_node(Tree,{9,9}),
    find_parent(Tree,MyNode),
    neighbours(Tree,MyNode),

    %% gs:create(rectangle,can1,[{coords,[{100,100},{200,200}]}]),

    I=gs:start(),
    Win=gs:create(window, I,
                  [{width, 1000},{height, 1000},
                   {title,"Default Demo"},{map, true}]),
    gs:create(canvas, can1,Win,
              [{x,0},{y, 0},{width,1000},{height,1000},
               {default,rectangle,{fill,black}},
               {default,oval,{fill,green}}]),
    %% gs:create(rectangle,can1,[{coords,[{100,100},{200,200}]}]),

    visualize(Tree),

    %% visualize(Tree),
    %% visualize(MyNode,blue),
    %% lists:map(
    %%   fun(Node) -> visualize(Node,yellow) end,
    %%   neighbours(Tree,MyNode)),

    Start = random_point(),
    Goal = random_point(),

    Way = astar(Tree,Start,Goal),
    if
        Way == failure -> ok;
        true ->
            lists:map(
              fun(Node) -> visualize(Node,yellow) end,
             Way)
    end,
    visualize(find_node(Tree,Start),blue),
    visualize(find_node(Tree,Goal),orange),

    ok.


random_point() ->
    {random:uniform(40)-20,random:uniform(40)-20}.


astar(Tree,StartPoint,GoalPoint) ->
    StartNode = find_node(Tree,StartPoint),
    GoalNode = find_node(Tree,GoalPoint),
    astar(Tree,GoalPoint,GoalNode,[],[{StartNode,0,[]}]).
astar_sort(Tree,GoalPoint,GoalNode,Closed,Open) ->
    SortedOpen = lists:sort(
      fun({Node1,_,_},{Node2,_,_}) ->
              min_dist2(Node1,GoalPoint) < min_dist2(Node2,GoalPoint)
      end,
      Open),
    astar(Tree,GoalPoint,GoalNode,Closed,SortedOpen).
astar(_,_,_,_,[]) ->
    failure;
astar(Tree,GoalPoint,GoalNode,Closed,[{Node,CostSoFar,PathSoFar}|Open]) ->
    IsGoalReached = eq_node(Node,GoalNode),
    if
        IsGoalReached -> [Node|PathSoFar];
        true ->
            OpenNeighbours =
                lists:filter(
                  fun(MaybeOpenNode) ->
                          not lists:any(
                                fun(ClosedNode) ->
                                        eq_node(MaybeOpenNode,ClosedNode)
                                end,
                                Closed)
                  end,
                  neighbours(Tree,Node)),

            NewOpen =
                lists:foldl(
                  fun(NeighbourNode,Open1) ->
                          NeighbourDist = node_dist2(Node,NeighbourNode),
                          AlreadyOpen =
                              lists:filter(
                                fun({OpenNode,_,_}) ->
                                        eq_node(NeighbourNode,OpenNode)
                                end,
                                Open1),
                          case AlreadyOpen of
                              [] ->
                                  [{NeighbourNode,
                                    CostSoFar+NeighbourDist,
                                    [Node|PathSoFar]}|Open1];
                              [OldNode] ->
                                  {OpenNode,OldCost,_} = OldNode,
                                  NewCost = CostSoFar+NeighbourDist,
                                  if
                                      NewCost < OldCost ->
                                          replace_node(
                                            {OpenNode,NewCost,[Node|PathSoFar]},
                                            Open1);
                                      true ->
                                          Open1
                                  end
                          end
                  end,
                  Open,
                  OpenNeighbours),
            astar_sort(Tree,GoalPoint,GoalNode,[Node|Closed],NewOpen)
    end.
            

replace_node(Item,List) ->
    replace_node(Item,List,[]).
replace_node(_,[],Res) ->
    Res;
replace_node(Item,[H|List],Res) ->
    {NewNode,_,_} = Item,
    {OldNode,_,_} = H,
    E = eq_node(NewNode,OldNode),
    if
        E -> replace_node(Item,List,[Item|Res]);
        true -> replace_node(Item,List,[H|Res])
    end.


insert_circle(Node,Circle) ->
    I = intersects_circle(Node,Circle),
    if
        I ->
%%            ?LOG({"insert_circle: intersection ",Node,Circle}),
            if
                is_list( Node#node.children ) ->
%%                    ?LOG("insert_circle: children found, recurse"),
                    Node#node{
                      children=lists:map(
                                 fun(ChildNode) ->
                                         insert_circle(ChildNode,Circle)
                                 end,
                                 Node#node.children)};
                true ->
%%                    ?LOG({"insert_circle: leafnode"}),
                    N = node_within_circle(Node,Circle),
                    if
                        (Node#node.size > ?MINSIZE)
                        and not N ->
%%                            ?LOG("insert_circle: create children, recurse"),
                            Node#node{
                              children=lists:map(
                                         fun(ChildNode) ->
                                                 insert_circle(ChildNode,Circle)
                                         end,
                                         new_children(Node)),
                              status=parent};
                        true ->
%%                            ?LOG("insert_circle: min size, set status obstacle"),
                            Node#node{status=obstacle}
                    end
            end;
        true -> Node
    end.

new_children(Node) ->
    NewSize = Node#node.size/2,
    [
     #node{
      x=Node#node.x+NewSize,
      y=Node#node.y+NewSize,
      size=NewSize
     },
                #node{
      x=Node#node.x+NewSize,
      y=Node#node.y-NewSize,
      size=NewSize
     },
                #node{
      x=Node#node.x-NewSize,
      y=Node#node.y+NewSize,
      size=NewSize
     },
                #node{
      x=Node#node.x-NewSize,
      y=Node#node.y-NewSize,
      size=NewSize
     }].

walk_tree(Node) ->
    if
        is_list(Node#node.children) ->
            io:format("~p ~p~n", [string:copies(" ",50-trunc(2*Node#node.size)),{Node#node.x,Node#node.y}]),
            lists:map(
              fun(ChildNode) ->
                      walk_tree(ChildNode)
              end,
              Node#node.children);
        true ->
            io:format("~p ~p~n", [string:copies(" ",50-trunc(2*Node#node.size)),{Node#node.x,Node#node.y}])
    end.

visualize(Node) ->
    visualize(Node,green).
visualize(Node,Color) ->
    if
        is_list(Node#node.children) ->
            lists:map(
              fun(ChildNode) ->
                      visualize(ChildNode)
              end,
              Node#node.children);
        true ->
            draw(Node,Color)
    end.

draw(Node,Color1) ->
    [{X1,Y1},{X2,Y2},_,_] = corners({Node#node.x,Node#node.y,Node#node.size}),
    case Node#node.status of
        obstacle -> Color = red;
        empty -> Color = Color1
    end,
    gs:create(rectangle,can1,
              [{coords,
                [{stretch(X1),stretch(Y1)},
                 {stretch(X2),stretch(Y2)}]},
               {fill,Color}]).

stretch(X) ->
    (X+20)*10.

find_node(Node,{X,Y}) ->
    %% io:format("find_node: ~p ~p~n", [string:copies(" ",50-trunc(2*Node#node.size)),{Node#node.x,Node#node.y}]),
    if
        is_list(Node#node.children) ->
            %% ?LOG({"find_node: children"}),
            [MyChild] = lists:filter(
              fun(ChildNode) ->
                      within_node(ChildNode,{X,Y})
              end,
              Node#node.children),
            find_node(MyChild,{X,Y});
        true -> 
            %% ?LOG({"find_node: leafnode"}),
            Node
    end.

find_parent(Node,Leaf) ->
    if
        is_list(Node#node.children) ->
            [MyChild] = lists:filter(
              fun(ChildNode) ->
                      within_node(ChildNode,{Leaf#node.x,Leaf#node.y})
              end,
              Node#node.children),
            if
                is_list(MyChild#node.children) ->
                    find_parent(MyChild,Leaf);
                true -> Node
            end;
        true -> throw(empty_tree)
    end.


%% within_node(Node,{X,Y}) ->
%%     (((Node#node.x-Node#node.size) < X)
%%      and (X =< (Node#node.x+Node#node.size)))
%%         and
%%           (((Node#node.y-Node#node.size) < Y)
%%            and (Y =< (Node#node.y+Node#node.size))).

within_node(Node,Point) ->
    within(Point,corners({Node#node.x,Node#node.y,Node#node.size})).

within({X,Y},[{X1,Y1},{X2,Y2},_,_]) ->
    ((X1 < X) and (X =< X2))
        and
          ((Y1 < Y) and (Y =< Y2)).

node_corners(Node) ->
    corners({Node#node.x,
             Node#node.y,
             Node#node.size}).

corners({X,Y,Size}) ->
    [{X-Size,Y-Size},
     {X+Size,Y+Size},
     {X+Size,Y-Size},
     {X-Size,Y+Size}].

intersects_node(Node1,Node2) ->
    C1 = corners({Node1#node.x,Node1#node.y,Node1#node.size}),
    C2 = corners({Node2#node.x,Node2#node.y,Node2#node.size}),
    lists:any(
      fun(Point) -> within(Point,C1) end,
      C2) or
        lists:any(
          fun(Point) -> within(Point,C2) end,
          C1).

node_within_circle(Node,Circle) ->
    lists:all(
      fun(Point) -> within_circle(Circle,Point) end,
      node_corners(Node)).

within_circle({X,Y,R},Point) ->
    dist2({X,Y},Point) =< sqr(R).

min_dist2(Node,Point) ->
    lists:min(
      lists:map(
        fun(NPoint) -> dist2(Point,NPoint) end,
        node_corners(Node))).

dist2({X1,Y1},{X2,Y2}) ->
    sqr(X1-X2) + sqr(Y1-Y2).

node_dist2(Node1,Node2) ->
    dist2({Node1#node.x,Node1#node.y},
          {Node2#node.x,Node2#node.y}).

eq_node(Node1,Node2) ->
    (((Node1#node.x == Node2#node.x)
      and (Node1#node.y == Node2#node.y))
     and (Node1#node.size == Node2#node.size)).


neighbours_old(Node,Leaf) ->
    neighbours_rec(Node,
                   %% Leaf#node{ size = Leaf#node.size - ?MINSIZE/2 }).
                   Leaf#node{ size = Leaf#node.size }).

neighbours(Node,Leaf) ->
    lists:filter(
      %% fun(ChildNode) -> not eq_node(Leaf,ChildNode) end,
      fun(ChildNode) ->
              Res = not lists:all(
                    fun(Point) -> within_node(Leaf,Point) end,
                    corners(
                      {ChildNode#node.x,
                       ChildNode#node.y,
                       ChildNode#node.size - ?MINSIZE/2})),
              %% ?LOG({"neighbours: within",Res}),
              Res
      end,
      neighbours_rec(Node,
                     Leaf#node{
                       x = Leaf#node.x + ?MINSIZE/2,
                       size = Leaf#node.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#node{
                       x = Leaf#node.x - ?MINSIZE/2,
                       size = Leaf#node.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#node{
                       y = Leaf#node.y + ?MINSIZE/2,
                       size = Leaf#node.size - ?MINSIZE/4
                      }) ++
      neighbours_rec(Node,
                     Leaf#node{
                       y = Leaf#node.y - ?MINSIZE/2,
                       size = Leaf#node.size - ?MINSIZE/4
                      })).


neighbours_rec(Node,Leaf) ->
    I = intersects_node(Node,Leaf),
    if
        I ->
            %% ?LOG({"neighbours: intersects"}),
            if
                is_list(Node#node.children) ->
                    %% ?LOG({"neighbours: children"}),
                    lists:flatmap(
                      fun(ChildNode) ->
                              neighbours_rec(ChildNode,Leaf)
                      end,
                      lists:filter(
                        fun(ChildNode) ->
                                intersects_node(ChildNode,Leaf)
                        end,
                        Node#node.children));
                true ->
                    %% ?LOG({"neighbours: leafnode"}),
                    if
                        Node#node.status == empty -> [Node];
                        true -> []
                    end
            end;
        true -> []
    end.


intersects_circle(Node,{X,Y,R}) ->
    NodeXmax = Node#node.x+Node#node.size,
    NodeXmin = Node#node.x-Node#node.size,
    NodeYmax = Node#node.y+Node#node.size,
    NodeYmin = Node#node.y-Node#node.size,

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

%%%     ?LOG({"intersects: ",
%%%           WithinHorizontally,
%%%           WithinVertically,
%%%           CloseVertically,
%%%           CloseHorizontally,
%%%           NodeXmax,
%%%           NodeXmin,
%%%           NodeYmax,
%%%           NodeYmin,
%%%           ok}),

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

sqr(X) ->
    X*X.
