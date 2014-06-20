"""
Additive Wave
by Daniel Shiffman. 

Create a more complex wave by adding two waves together. 
"""

xspacing = 8    # How far apart should each horizontal location be spaced
maxwaves = 4    # total # of waves to add together
theta = 0.0

amplitude = []  # Height of wave
# Value for incrementing X, to be calculated as a function of period and
# xspacing
dx = []
yvalues = []


def setup():
    size(640, 360)
    frameRate(30)
    colorMode(RGB, 255, 255, 255, 100)
    w = width + 16
    for i in range(maxwaves):
        amplitude.append(random(10, 30))
        period = random(100, 300)  # How many pixels before the wave repeats
        dx.append((TWO_PI / period) * xspacing)
    for _ in range(w / xspacing + 1):
        yvalues.append(0.0)


def draw():
    background(0)
    calcWave()
    renderWave()


def calcWave():
    # Increment theta (try different values for 'angular velocity' here
    theta += 0.02
    # Set all height values to zero
    for i in range(len(yvalues)):
        yvalues[i] = 0
    # Accumulate wave height values
    for j in range(maxwaves):
        x = theta
        for i in range(len(yvalues)):
            # Every other wave is cosine instead of sine
            if j % 2 == 0:
                yvalues[i] += sin(x) * amplitude[j]
            else:
                yvalues[i] += cos(x) * amplitude[j]
            x += dx[j]


def renderWave():
    # A simple way to draw the wave with an ellipse at each location
    noStroke()
    fill(255, 50)
    ellipseMode(CENTER)
    for x, v in enumerate(yvalues):
        ellipse(x * xspacing, height / 2 + v, 16, 16)

