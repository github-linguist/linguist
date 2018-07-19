# Conversion from Grey to RGB uses the following code
setAs("pixmapGrey", "pixmapRGB",
function(from, to){
    z = new(to, as(from, "pixmap"))
    z@red = from@grey
    z@green = from@grey
    z@blue = from@grey
    z@channels = c("red", "green", "blue")
    z
})

# Conversion from RGB to grey uses built-in coefficients of 0.3, 0.59, 0.11.  To see this, type
getMethods(addChannels)

# We can override this behaviour with
setMethod("addChannels", "pixmapRGB",
function(object, coef=NULL){
    if(is.null(coef)) coef = c(0.2126, 0.7152, 0.0722)
    z = new("pixmapGrey", object)
    z@grey = coef[1] * object@red + coef[2] * object@green +
        coef[3] * object@blue
    z@channels = "grey"
    z
})

# Colour image
plot(p1 <- pixmapRGB(c(c(1,0,0,0,0,1), c(0,1,0,0,1,0), c(0,0,1,1,0,0)), nrow=6, ncol=6))

#Convert to grey
plot(p2 <- as(p1, "pixmapGrey"))

# Convert back to "colour"
plot(p3 <- as(p2, "pixmapRGB"))
