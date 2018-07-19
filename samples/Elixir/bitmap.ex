defmodule RosBitmap do
  defrecord Bitmap, pixels: nil, shape: {0, 0}

  defp new(width, height, {:rgb, r, g, b}) do
    Bitmap[
      pixels: :array.new(width * height,
        {:default, <<r::size(8), g::size(8), b::size(8)>>}),
      shape: {width, height}]
  end

  def new(width, height), do: new(width, height, {:rgb, 0, 0, 0})

  def fill(Bitmap[shape: {width, height}], {:rgb, _r, _g, _b}=color) do
    new(width, height, color)
  end

  def set_pixel(Bitmap[pixels: pixels, shape: {width, _height}]=bitmap,
      {:at, x, y}, {:rgb, r, g, b}) do
    index = x + y * width
    bitmap.pixels(:array.set(index, <<r::size(8), g::size(8), b::size(8)>>, pixels))
  end

  def get_pixel(Bitmap[pixels: pixels, shape: {width, _height}], {:at, x, y}) do
    index = x + y * width
    <<r::size(8), g::size(8), b::size(8)>> = :array.get(index, pixels)
    {:rgb, r, g, b}
  end
end
