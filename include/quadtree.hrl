-record(quadtree, {
          x,
          y,
          size,
          children=undefined,
          status=empty
}).

-define(MINSIZE, 5).
