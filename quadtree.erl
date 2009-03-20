-module(quadtree).
-compile(export_all).
%%-export([bench/0]).


%% -ifdef(debug).
%% -define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).
%% -else.
%% -define(LOG(Msg), true).
%% -endif.
-define(LOG(Msg), io:format("{~p:~p}: ~p~n", [?MODULE, ?LINE, Msg])).

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
      children=[],
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


    _ = ListOfCircles,
%%%     lists:map(
%%%       fun(Circle) 
%%%          -> add_to_quadtree(QuadTree,Circle)
%%%       end,
%%%       ListOfCircles).
    ok.


add_to_quadtree(QuadTree,Circle) ->
    intersects(QuadTree,Circle),
    %%     if object intersects node
    %%         if node is leaf
    %%             if node.depth > max
    %%                 node.status = obstacle
    %%             else
    %%                 create 4 new children
    %%                 recurse for all children
    %%         recurse for all children
    QuadTree.











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
    





coords_to_pov(Ox,Oy,Dir,{X,Y}) ->
    X1 = X-Ox,
    Y1 = Y-Oy,
    Sin = math:sin(Dir),
    Cos = math:cos(Dir),
    X2 = X1*Cos-Y1*Sin,
    Y2 = Y1*Cos+X1*Sin,
    %%?LOG({"coords_to_pov: ",{Ox,Oy},Dir,{X,Y},{X1,Y1},{sin,Sin},{cos,Cos},{X2,Y2}}),
    {X2,Y2}.



sqr(X) ->
    X*X.

str2num(Str) ->
    {Num,_} = string:to_float(Str),
    Num.
