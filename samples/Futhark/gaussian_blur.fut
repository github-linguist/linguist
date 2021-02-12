-- # Gaussian blur
--
-- One common pattern of array computation is the so-called
-- [stencil](https://en.wikipedia.org/wiki/Stencil_code), where we
-- change the value of an element in the array based on its
-- neighbours.  For example, we might implement image blurring by
-- assigning each pixel the average value of all of its neighbors.
-- Futhark does not have a special-purpose stencil language construct.
-- Instead, stencil computations are expressed as `map`s on the index
-- space, using explicit array indexing to access the stencil source
-- array and returning the new value for the index.  While this is
-- rather verbose, at least until Futhark grows more syntactical
-- conveniences, it works and performs well.  Let's look at how to
-- implement a simple image blurring program.
--
-- We will represent an image as a three-dimensional array
-- `[rows][cols][3]u8`.  The innermost size-3 dimension encodes the
-- three colour channels for red, green, and blue, respectively.  When
-- blurring, it is useful to operate on each colour channel separately.
-- Furthermore, instead of the colour being a number from 0 to 255, it is
-- more convenient to store it as a floating-point number between 0 and
-- 1.0.  Therefore, we define a function that transforms an array of type
-- `[rows][cols][3]u8` into three arrays of type
-- `[rows][cols]f32` each.  The result is that we have one array for
-- each of the three colour channels:
--
let split_channels [rows][cols]
                   (image: [rows][cols][3]u8): ([rows][cols]f32,
                                                [rows][cols]f32,
                                                [rows][cols]f32) =
  unzip3 (map (\row ->
                unzip3 (map (\pixel ->
                              (f32.u8 pixel[0] / 255,
                               f32.u8 pixel[1] / 255,
                               f32.u8 pixel[2] / 255))
                            row))
              image)

-- The `[rows][cols]` notation preceding the `image` parameter is not
-- a normal function parameter.  Rather, it is a *size parameter*, a way
-- of indicating that the function `split_channels` is polymorphic
-- in the sizes `rows` and `cols`.  The main purpose is that we can
-- then use these names to indicate the sizes of the parameter and return
-- values of the function.  When the function is called, size parameters
-- need not be passed arguments explicitly, but are automatically
-- inferred from the concrete `image` argument.  If we did not
-- explicitly add these size parameters, the Futhark compiler would look
-- for variables `rows` and `cols` in scope.
--
-- The function `split_channels` maps across each inner `[3]u8`
-- element (`pixel`), turns this into a triple instead of a
-- three-element array, then uses `unzip` to turn the resulting
-- array-of-triples into a triple-of-arrays, which is then returned.  For
-- readability, we could have chosen to explicitly indicate the return
-- and parameter types of the anonymous function, but in the interest of
-- brevity we have left them for the compiler to infer.  It is only
-- required to explicitly indicate the types of all top-level functions.
--
-- We will also need to re-combine the colour channel arrays into a
-- single array.  That function looks like this:

let combine_channels [rows][cols]
                     (rs: [rows][cols]f32)
                     (gs: [rows][cols]f32)
                     (bs: [rows][cols]f32): [rows][cols][3]u8 =
  map3 (\rs_row gs_row bs_row ->
         map3 (\r g b ->
                [u8.f32 (r * 255),
                 u8.f32 (g * 255),
                 u8.f32 (b * 255)])
              rs_row gs_row bs_row)
       rs gs bs

-- Another thing we will need is the actual stencil function.  That is,
-- the function we wish to apply to every pixel in the image.  For
-- blurring, we will take the average value of the pixel itself plus each
-- of its eight neighbors (nine values in total):

let new_value [rows][cols]
             (image: [rows][cols]f32) (row: i32) (col: i32): f32 =
  unsafe
  let sum =
    image[row-1,col-1] + image[row-1,col] + image[row-1,col+1] +
    image[row,  col-1] + image[row,  col] + image[row,  col+1] +
    image[row+1,col-1] + image[row+1,col] + image[row+1,col+1]
  in sum / 9

-- The function call `new_value(image, row, col)` computes the new value
-- for the pixel at position `(row, col)` in `image`.
--
-- The alert reader will have noticed that `new_value` cannot be applied
-- to pixels on the edge of the image - doing so would result in
-- out-of-bounds accesses to the `image` array.  We will take care to
-- only call the `new_value` function with safe indices, but the Futhark
-- compiler is sadly not yet smart enough to realise this - thus we are
-- forced to use the `unsafe` keyword to prevent the insertion of
-- bounds checks that would otherwise hinder parallelisation.  If we did
-- not use `unsafe`, the Futhark compiler would fail with an error
-- message pointing at the problematic array access.
--
-- Now we can write the actual stencil function, which applies
-- `new_value` to every inner element of a colour channel array.  This
-- uses the `iota` function for constructing an array of integers
-- ranging from *0* to the provided argument (the latter not inclusive).
-- The edges are left unchanged:

let blur [rows][cols]
         (channel: [rows][cols]f32): [rows][cols]f32 =
  map (\row ->
         map(\col ->
               if row > 0 && row < rows-1 && col > 0 && col < cols-1
               then new_value channel row col
               else channel[row,col])
             (iota cols))
      (iota rows)

-- You may have heard that branches are expensive on a GPU.  While this
-- is a good basic rule of thumb, what is actually expensive is *branch
-- divergence* - that is, when neighboring threads take *different* paths
-- through a branch.  In our stencil, only the edge elements will take
-- the false branch, and these are few in number compared to the
-- interior.
--
-- Stencil computations usually have an outer (sequential) loop for
-- applying the stencil several times.  Our program is no different - we
-- will apply the blurring transformation a user-defined number of times.
-- The more iterations we run, the more blurred the image will become:

let main [rows][cols]
         (iterations: i32) (image: [rows][cols][3]u8): [rows][cols][3]u8 =
  let (rs, gs, bs) = split_channels image
  let (rs, gs, bs) = loop (rs, gs, bs) for i < iterations do
    let rs = blur rs
    let gs = blur gs
    let bs = blur bs
    in (rs, gs, bs)
  in combine_channels rs gs bs

-- Our `main` function is quite simple.  We split the input image into
-- three different channels, use a sequential loop to blur each colour
-- channel the requested number of times, then recombine the resulting
-- channel arrays into a single final image.
--
-- The Futhark `loop` construct merits an explanation: in the above
-- function, we declare three *loop variant variables*, `rs`, `gs`,
-- and `bs`.  These take their initial values from the incidentally
-- identically named variables in scope (but this is not in general
-- requirement).  The *loop body* then returns three values that become
-- the values of the loop variant variables in the next iteration of the
-- loop.  In essence, the `loop` construct is just syntactical suger
-- for a particularly simple (but common) pattern of tail-recursive
-- function.  However, the Futhark compiler is able to perform
-- transformations involving `loop`s that it cannot for recursive
-- functions (although it does not perform any such for this simple
-- program).
--
-- The three separate calls to `blur` may seem wasteful, but the
-- Futhark compiler is smart enough to fuse them together into a single
-- GPU kernel that traverses the three colour channel arrays
-- simultaneously.  This is an instance of *horisontal fusion*.
--
-- Our Futhark program is now done.  We can make it a little more
-- useful by writing a small Python wrapper program for reading and
-- writing PNGs: [blur-png.py](/static/blur-png.py).  We must
-- compile `blur.fut` using the PyOpenCL backend:
--
-- ```
-- $ futhark pyopencl --library blur.fut
-- ```
--
-- This produces a Python module `blur.py` which is then imported by
-- `blur-png.py`.  We can try it out on any PNG image, say, this
-- [illustration of the spirit of Futhark](/images/gottagofast.png):
--
-- ```
-- $ python blur-png.py gottagofast.png --output-file gottagofast-blurred.png
-- ```
--
-- Which produces [this slightly smushed
-- image](/images/gottagofast-blurred.png).  We can also ask for a
-- hundred iterations::
--
-- ```
-- $ python blur-png.py gottagofast.png --output-file gottagofast-blurred.png --iterations 100
-- ```
--
-- Which produces [this blurry
-- mess](/images/gottagofast-veryblurred.png).  Notice the edges -
-- perhaps simply keeping them unchanged is not the best way to
-- implement image blurring.  Still, this program is a decent
-- description of how to implement stencils in Futhark.
