-module(map).
-export([start/0]).

-include("../include/debug.hrl").
-include("../include/map.hrl").


start() ->
    receive
        {start, {MapQuad, Steer}} ->
            Map = #map{},
            loop(MapQuad, Steer, Map)
    end.

loop(MapQuad, Steer, Map) ->
    receive
        {boulder, Boulder} ->
            AlreadyKnown = lists:member(Boulder, Map#map.boulders),
            if
                AlreadyKnown -> loop(MapQuad, Steer, Map);
                true ->
                    Map1 = Map#map{
                             boulders=[Boulder|Map#map.boulders] },
                    MapQuad ! {new, Boulder},
                    loop(MapQuad, Steer, Map1)
            end;
        {crater, Crater} ->
            AlreadyKnown = lists:member(Crater, Map#map.craters),
            if
                AlreadyKnown -> loop(MapQuad, Steer, Map);
                true ->
                    Map1 = Map#map{
                             craters=[Crater|Map#map.craters] },
                    MapQuad ! {new, Crater},
                    loop(MapQuad, Steer, Map1)
            end;
        {alien, Alien} ->
            ?LOG({"map:loop aliens currently unhandled", Alien}),
            loop(MapQuad, Steer, Map);
        Any ->
            ?LOG({"map:loop received unknown msg", Any}),
            loop(MapQuad, Steer, Map)
    end.

