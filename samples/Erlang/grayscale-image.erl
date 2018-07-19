-module(ros_bitmap).

-export([new/2, fill/2, set_pixel/3, get_pixel/2, convert/2]).

-record(bitmap, {
    mode = rgb,
    pixels = nil,
    shape = {0, 0}
  }).

tuple_to_bytes({rgb, R, G, B}) ->
  <<R:8, G:8, B:8>>;
tuple_to_bytes({gray, L}) ->
  <<L:8>>.

bytes_to_tuple(rgb, Bytes) ->
  <<R:8, G:8, B:8>> = Bytes,
  {rgb, R, G, B};
bytes_to_tuple(gray, Bytes) ->
  <<L:8>> = Bytes,
  {gray, L}.

new(Width, Height) ->
  new(Width, Height, {rgb, 0, 0, 0}).

new(Width, Height, rgb) ->
  new(Width, Height, {rgb, 0, 0, 0});

new(Width, Height, gray) ->
  new(Width, Height, {gray, 0, 0, 0});

new(Width, Height, ColorTuple) when is_tuple(ColorTuple) ->
  [Mode|Components] = tuple_to_list(ColorTuple),
  Bytes = list_to_binary(Components),
  #bitmap{
    pixels=array:new(Width * Height, {default, Bytes}),
    shape={Width, Height},
    mode=Mode}.

fill(#bitmap{shape={Width, Height}, mode=Mode}, ColorTuple)
    when element(1, ColorTuple) =:= Mode ->
  new(Width, Height, ColorTuple).

set_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}, mode=Mode}=Bitmap,
    {at, X, Y}, ColorTuple) when  element(1, ColorTuple) =:= Mode ->
  Index = X + Y * Width,
  Bitmap#bitmap{pixels=array:set(Index, tuple_to_bytes(ColorTuple), Pixels)}.

get_pixel(#bitmap{pixels=Pixels, shape={Width, _Height}, mode=Mode},
    {at, X, Y}) ->
  Index = X + Y * Width,
  Bytes = array:get(Index, Pixels),
  bytes_to_tuple(Mode, Bytes).

luminance(<<R:8, G:8, B:8>>) ->
  <<(trunc(R * 0.2126 + G * 0.7152 + B * 0.0722))>>.

%% convert from rgb to grayscale
convert(#bitmap{pixels=Pixels, mode=rgb}=Bitmap, gray) ->
  Bitmap#bitmap{
    pixels=array:map(fun(_I, Pixel) ->
          luminance(Pixel) end, Pixels),
    mode=gray};

%% convert from grayscale to rgb
convert(#bitmap{pixels=Pixels, mode=gray}=Bitmap, rgb)->
  Bitmap#bitmap{
    pixels=array:map(fun(_I, <<L:8>>) -> <<L:8, L:8, L:8>> end, Pixels),
    mode=rgb};

%% no conversion if the mode is the same with the bitmap.
convert(#bitmap{mode=Mode}=Bitmap, Mode) ->
  Bitmap.
