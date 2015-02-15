void convertToGrayscale(final BufferedImage image){
    for(int i=0; i<image.getWidth(); i++){
        for(int j=0; j<image.getHeight(); j++){
            int color = image.getRGB(i,j);

            int alpha = (color >> 24) & 255;
            int red = (color >> 16) & 255;
            int green = (color >> 8) & 255;
            int blue = (color) & 255;

            final int lum = (int)(0.2126 * red + 0.7152 * green + 0.0722 * blue);

            alpha = (alpha << 24);
            red = (lum << 16);
            green = (lum << 8);
            blue = lum;

            color = alpha + red + green + blue;

            image.setRGB(i,j,color);
        }
    }
}
