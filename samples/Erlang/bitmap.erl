-module(ros_bitmap).

-export([new/2, fill/2, set_pixel/3, get_pixel/2]).

-record(bitmap, {
    pixels = nil,
    shape = {0, 0}
  }).

new(Width, Height) ->
  #bitmap{pixels=array:new(Width * Height, {default, <<0:8, 0:8, 0:8>>}), shape={Width, Height}}.

fill(#bitmap{shape={Width, Height}}, {rgb, R, G, B}) ->
  #bitmap{
    pixels=array:new(Width * Height, {default, <<R:8, G:8, B:8>>}),
    shape={Width, Height}}.

set_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}}=Bitmap, {at, X, Y}, {rgb, R, G, B}) ->
  Index = X + Y * Width,
  Bitmap#bitmap{pixels=array:set(Index, <<R:8, G:8, B:8>>, Pixels)}.

get_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}}, {at, X, Y}) ->
  Index = X + Y * Width,
  <<R:8, G:8, B:8>> = array:get(Index, Pixels),
  {rgb, R, G, B}.
