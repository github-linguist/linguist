#include <cstdlib>
#include <complex>

// get dimensions for arrays
template<typename ElementType, std::size_t dim1, std::size_t dim2>
 std::size_t get_first_dimension(ElementType (&a)[dim1][dim2])
{
  return dim1;
}

template<typename ElementType, std::size_t dim1, std::size_t dim2>
 std::size_t get_second_dimension(ElementType (&a)[dim1][dim2])
{
  return dim2;
}


template<typename ColorType, typename ImageType>
 void draw_Mandelbrot(ImageType& image,                                   //where to draw the image
                      ColorType set_color, ColorType non_set_color,       //which colors to use for set/non-set points
                      double cxmin, double cxmax, double cymin, double cymax,//the rect to draw in the complex plane
                      unsigned int max_iterations)                          //the maximum number of iterations
{
  std::size_t const ixsize = get_first_dimension(ImageType);
  std::size_t const iysize = get_first_dimension(ImageType);
  for (std::size_t ix = 0; ix < ixsize; ++ix)
    for (std::size_t iy = 0; iy < iysize; ++iy)
    {
      std::complex<double> c(cxmin + ix/(ixsize-1.0)*(cxmax-cxmin), cymin + iy/(iysize-1.0)*(cymax-cymin));
      std::complex<double> z = 0;
      unsigned int iterations;

      for (iterations = 0; iterations < max_iterations && std::abs(z) < 2.0; ++iterations)
        z = z*z + c;

      image[ix][iy] = (iterations == max_iterations) ? set_color : non_set_color;

    }
}
