-module(pathfind).
-export([start/0]).

-include("/home/lefant/shared/code/erlang/mars-rover/debug.hrl").
-include("/home/lefant/shared/code/erlang/mars-rover/quadtree.hrl").



test() ->
    ok.

start() ->
    loop().

loop() ->
    receive
        Any ->
            ?LOG({"pathfind loop: unknown msg",Any}),
            loop()
    end.




next_subgoal([]) ->
    {{0,0},[]};
next_subgoal([GoalNode]) ->
    ?LOG({"next_subgoal, final subgoal: ",{GoalNode#quadtree.x,GoalNode#quadtree.y}}),
    {{0,0},[]};
next_subgoal([LastNode,NextNode]) ->
    next_subgoal([LastNode,NextNode,#quadtree{x=0,y=0,size=20}]);
next_subgoal([LastNode,CurNode,NextNode|Path]) ->
    %% N = {NextNode#quadtree.x,NextNode#quadtree.y},

    NextGoal = midpoint(
        nodes_touching(LastNode,CurNode),
        nodes_touching(CurNode,NextNode)),

    {NextGoal,[NextNode|Path]}.



astar(Tree,StartPoint,GoalPoint) ->
    StartNode = find_node(Tree,StartPoint),
    GoalNode = find_node(Tree,GoalPoint),
    E = eq_node(StartNode,GoalNode),
    if
        E -> [StartNode];
        true -> astar(Tree,GoalPoint,GoalNode,[],[{StartNode,0,[]}])
    end.
astar(_,_,_,_,[]) ->
    failure;
astar(Tree,GoalPoint,GoalNode,Closed,[{Node,CostSoFar,PathSoFar}|Open]) ->
    IsGoalReached = eq_node(Node,GoalNode),
    if
        IsGoalReached ->
            lists:reverse([Node|PathSoFar]);
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

            SortedOpen =
                lists:sort(
                  fun({Node1,_,_},{Node2,_,_}) ->
                          min_dist2(Node1,GoalPoint) < min_dist2(Node2,GoalPoint)
                  end,
                  NewOpen),

            astar(Tree,GoalPoint,GoalNode,[Node|Closed],SortedOpen)
    end.
            







insert_circle(Node,Circle) ->
    I = intersects_circle(Node,Circle),
    if
        I ->
            if
                is_list( Node#quadtree.children ) ->
                    Node#quadtree{
                      children=lists:map(
                                 fun(ChildNode) ->
                                         insert_circle(ChildNode,Circle)
                                 end,
                                 Node#quadtree.children)};
                true ->
                    N = node_within_circle(Node,Circle),
                    O = Node#quadtree.status=:=obstacle,
                    if
                        (((Node#quadtree.size > ?MINSIZE)
                        and not N) and not O) ->
                            Node#quadtree{
                              children=lists:map(
                                         fun(ChildNode) ->
                                                 insert_circle(ChildNode,Circle)
                                         end,
                                         new_children(Node)),
                              status=parent};
                        true ->
                            visualize(Node,red),
                            Node#quadtree{status=obstacle}
                    end
            end;
        true ->
            Node
    end.







%%%%% graphics for demo / debugging purposes

% visualize/2 takes a Node or list of Nodes and a color
% will then recurse if necessary and draw it all
visualize([],_) ->
    ok;
visualize([Node|List],Color) ->
    visualize(Node,Color),
    visualize(List,Color);
visualize(Node,Color) ->
    if
        is_list(Node#quadtree.children) ->
            lists:map(
              fun(ChildNode) ->
                      visualize(ChildNode,Color)
              end,
              Node#quadtree.children);
        true ->
            [X1,X2,_,_] = node_corners(Node),
            case Node#quadtree.status of
                obstacle -> Color1 = red;
                empty -> Color1 = Color
            end,
            visualizer ! {node,{X1,X2},Color1}
    end.





