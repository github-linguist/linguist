#include <iostream>
#include <boost/gil/gil_all.hpp>
int main()
{
    using namespace boost::gil;
    // create 30x40 image
    rgb8_image_t img(30, 40);

    // fill with red
    rgb8_pixel_t red(255, 0, 0);
    fill_pixels(view(img), red);

    // set pixel at 10x20 to blue
    rgb8_pixel_t blue(0, 0, 255);
    view(img)(10, 20) = blue;

    // read the value of pixel at 11x20
    rgb8_pixel_t px = const_view(img)(11, 20);
    std::cout << "the pixel at 11, 20 is " << (unsigned)px[0] << ':' << (unsigned)px[1] << ':' << (unsigned)px[2]  << '\n';
}
