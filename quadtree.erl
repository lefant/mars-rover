-module(quadtree).
-compile(export_all).
%%-export([bench/0]).


%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.
-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).

-define(MINSIZE, 1).

-record(node, {
          x,
          y,
          size,
          children,
          status
}).

test() ->
    ?LOG("Debug is enabled"),
    dbg:tracer(),
    dbg:p(all, call),
%%%     dbg:tpl(quadtree, intersects, 2, []),
%%%     dbg:tpl(mtm, inner_prod, 4, []),
%%%     dbg:tpl(mtm, receive_data, 3, []),

    ListOfCircles = [
                     {-10,-5,3},
                     {3,-4,2},
                     {17,13,5}
                     ],
    QuadTree = #node{
      x=0,
      y=0,
      size=20,
      children=undefined,
      status=empty
     },

    false = intersects(QuadTree,{100,100,1}),
    false = intersects(QuadTree,{-100,-100,1}),
    false = intersects(QuadTree,{100,-100,1}),
    false = intersects(QuadTree,{-100,100,1}),

    false = intersects(QuadTree,{100,100,85}),
    false = intersects(QuadTree,{-100,100,85}),
    false = intersects(QuadTree,{100,-100,85}),
    false = intersects(QuadTree,{-100,-100,85}),

    false = intersects(QuadTree,{100,0,1}),
    false = intersects(QuadTree,{0,100,1}),
    false = intersects(QuadTree,{-100,0,1}),
    false = intersects(QuadTree,{0,-100,1}),


    true = intersects(QuadTree,{0,0,10}),

    true = intersects(QuadTree,{25,0,10}),
    true = intersects(QuadTree,{-25,0,10}),
    true = intersects(QuadTree,{0,25,10}),
    true = intersects(QuadTree,{0,-25,10}),

    true = intersects(QuadTree,{25,25,10}),
    true = intersects(QuadTree,{-25,25,10}),
    true = intersects(QuadTree,{25,-25,10}),
    true = intersects(QuadTree,{-25,-25,10}),

    Tree = lists:foldl(
        fun(Circle,Node)
           -> insert_circle(Node,Circle)
        end,
        QuadTree,
        ListOfCircles),
    walk_tree(Tree),

    walk_tree(
      insert_circle(QuadTree,{3,-4,2})),

    ok.


insert_circle(Node,Circle) ->
    I = intersects(Node,Circle),
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
                    if 
                        Node#node.size > ?MINSIZE ->
%%                            ?LOG("insert_circle: create children, recurse"),
                            Node#node{
                              children=lists:map(
                                         fun(ChildNode) ->
                                                 insert_circle(ChildNode,Circle)
                                         end,
                                         new_children(Node))};
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








intersects(Node,{X,Y,R}) ->
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
