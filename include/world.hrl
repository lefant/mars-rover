-record(world, {
          home = {0.000,0.000,5.000},

          width = 100,
          height = 100,
          timelimit = 30000,
          minsensor = 20,
          maxsensor = 40,
          maxspeed = 60,
          maxturn = 20,
          maxhardturn = 60,


%%% all this should go


% elsewhere
          path=[],
          curnode,
          goal={0,0},
          quadtree
         }).
