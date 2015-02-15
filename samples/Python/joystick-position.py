import sys
import pygame

pygame.init()

# Create a clock (for framerating)
clk = pygame.time.Clock()

# Grab joystick 0
if pygame.joystick.get_count() == 0:
    raise IOError("No joystick detected")
joy = pygame.joystick.Joystick(0)
joy.init()

# Create display
size = width, height = 600, 600
screen = pygame.display.set_mode(size)
pygame.display.set_caption("Joystick Tester")

# Frame XHair zone
frameRect = pygame.Rect((45, 45), (510, 510))

# Generate crosshair
crosshair = pygame.surface.Surface((10, 10))
crosshair.fill(pygame.Color("magenta"))
pygame.draw.circle(crosshair, pygame.Color("blue"), (5,5), 5, 0)
crosshair.set_colorkey(pygame.Color("magenta"), pygame.RLEACCEL)
crosshair = crosshair.convert()

# Generate button surfaces
writer = pygame.font.Font(pygame.font.get_default_font(), 15)
buttons = {}
for b in range(joy.get_numbuttons()):
    buttons[b] = [
        writer.render(
            hex(b)[2:].upper(),
            1,
            pygame.Color("red"),
            pygame.Color("black")
        ).convert(),
        # Get co-ords: ((width*slot)+offset, offset). Offsets chosen
        #                                             to match frames.
        ((15*b)+45, 560)
    ]

while True:
    # Pump and check the events queue
    pygame.event.pump()
    for events in pygame.event.get():
        if events.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

    # Black the screen
    screen.fill(pygame.Color("black"))

    # Get joystick axes
    x = joy.get_axis(0)
    y = joy.get_axis(1)

    # Blit to the needed coords:
    # x*amplitude+(centre offset (window size/2))-(xhair offset (xh size/2))
    screen.blit(crosshair, ((x*250)+300-5, (y*250)+300-5))
    pygame.draw.rect(screen, pygame.Color("red"), frameRect, 1)

    # Get and display the joystick buttons
    for b in range(joy.get_numbuttons()):
        if joy.get_button(b):
            screen.blit(buttons[b][0], buttons[b][1])

    # Write the display
    pygame.display.flip()
    clk.tick(40) # Limit to <=40 FPS
