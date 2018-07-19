import pygame, sys
from pygame.locals import *
from math import sin, cos, radians

pygame.init()

WINDOWSIZE = 250
TIMETICK = 100
BOBSIZE = 15

window = pygame.display.set_mode((WINDOWSIZE, WINDOWSIZE))
pygame.display.set_caption("Pendulum")

screen = pygame.display.get_surface()
screen.fill((255,255,255))

PIVOT = (WINDOWSIZE/2, WINDOWSIZE/10)
SWINGLENGTH = PIVOT[1]*4

class BobMass(pygame.sprite.Sprite):
    def __init__(self):
        pygame.sprite.Sprite.__init__(self)
        self.theta = 45
        self.dtheta = 0
        self.rect = pygame.Rect(PIVOT[0]-SWINGLENGTH*cos(radians(self.theta)),
                                PIVOT[1]+SWINGLENGTH*sin(radians(self.theta)),
                                1,1)
        self.draw()

    def recomputeAngle(self):
        scaling = 3000.0/(SWINGLENGTH**2)

        firstDDtheta = -sin(radians(self.theta))*scaling
        midDtheta = self.dtheta + firstDDtheta
        midtheta = self.theta + (self.dtheta + midDtheta)/2.0

        midDDtheta = -sin(radians(midtheta))*scaling
        midDtheta = self.dtheta + (firstDDtheta + midDDtheta)/2
        midtheta = self.theta + (self.dtheta + midDtheta)/2

        midDDtheta = -sin(radians(midtheta)) * scaling
        lastDtheta = midDtheta + midDDtheta
        lasttheta = midtheta + (midDtheta + lastDtheta)/2.0

        lastDDtheta = -sin(radians(lasttheta)) * scaling
        lastDtheta = midDtheta + (midDDtheta + lastDDtheta)/2.0
        lasttheta = midtheta + (midDtheta + lastDtheta)/2.0

        self.dtheta = lastDtheta
        self.theta = lasttheta
        self.rect = pygame.Rect(PIVOT[0]-
                                SWINGLENGTH*sin(radians(self.theta)),
                                PIVOT[1]+
                                SWINGLENGTH*cos(radians(self.theta)),1,1)


    def draw(self):
        pygame.draw.circle(screen, (0,0,0), PIVOT, 5, 0)
        pygame.draw.circle(screen, (0,0,0), self.rect.center, BOBSIZE, 0)
        pygame.draw.aaline(screen, (0,0,0), PIVOT, self.rect.center)
        pygame.draw.line(screen, (0,0,0), (0, PIVOT[1]), (WINDOWSIZE, PIVOT[1]))

    def update(self):
        self.recomputeAngle()
        screen.fill((255,255,255))
        self.draw()

bob = BobMass()

TICK = USEREVENT + 2
pygame.time.set_timer(TICK, TIMETICK)

def input(events):
    for event in events:
        if event.type == QUIT:
            sys.exit(0)
        elif event.type == TICK:
            bob.update()

while True:
    input(pygame.event.get())
    pygame.display.flip()
