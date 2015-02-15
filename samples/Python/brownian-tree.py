import pygame, sys, os
from pygame.locals import *
from random import randint
pygame.init()

MAXSPEED = 15
SIZE = 3
COLOR = (45, 90, 45)
WINDOWSIZE = 400
TIMETICK = 1
MAXPART = 50

freeParticles = pygame.sprite.Group()
tree = pygame.sprite.Group()

window = pygame.display.set_mode((WINDOWSIZE, WINDOWSIZE))
pygame.display.set_caption("Brownian Tree")

screen = pygame.display.get_surface()


class Particle(pygame.sprite.Sprite):
    def __init__(self, vector, location, surface):
        pygame.sprite.Sprite.__init__(self)
        self.vector = vector
        self.surface = surface
        self.accelerate(vector)
        self.add(freeParticles)
        self.rect = pygame.Rect(location[0], location[1], SIZE, SIZE)
        self.surface.fill(COLOR, self.rect)

    def onEdge(self):
        if self.rect.left <= 0:
            self.vector = (abs(self.vector[0]), self.vector[1])
        elif self.rect.top <= 0:
            self.vector = (self.vector[0], abs(self.vector[1]))
        elif self.rect.right >= WINDOWSIZE:
            self.vector = (-abs(self.vector[0]), self.vector[1])
        elif self.rect.bottom >= WINDOWSIZE:
            self.vector = (self.vector[0], -abs(self.vector[1]))

    def update(self):
        if freeParticles in self.groups():
            self.surface.fill((0,0,0), self.rect)
            self.remove(freeParticles)
            if pygame.sprite.spritecollideany(self, freeParticles):
                self.accelerate((randint(-MAXSPEED, MAXSPEED),
                                 randint(-MAXSPEED, MAXSPEED)))
                self.add(freeParticles)
            elif pygame.sprite.spritecollideany(self, tree):
                self.stop()
            else:
                self.add(freeParticles)

            self.onEdge()

            if (self.vector == (0,0)) and tree not in self.groups():
                self.accelerate((randint(-MAXSPEED, MAXSPEED),
                                 randint(-MAXSPEED, MAXSPEED)))
            self.rect.move_ip(self.vector[0], self.vector[1])
        self.surface.fill(COLOR, self.rect)

    def stop(self):
        self.vector = (0,0)
        self.remove(freeParticles)
        self.add(tree)

    def accelerate(self, vector):
        self.vector = vector

NEW = USEREVENT + 1
TICK = USEREVENT + 2

pygame.time.set_timer(NEW, 50)
pygame.time.set_timer(TICK, TIMETICK)


def input(events):
    for event in events:
        if event.type == QUIT:
            sys.exit(0)
        elif event.type == NEW and (len(freeParticles) < MAXPART):
            Particle((randint(-MAXSPEED,MAXSPEED),
                      randint(-MAXSPEED,MAXSPEED)),
                     (randint(0, WINDOWSIZE), randint(0, WINDOWSIZE)),
                     screen)
        elif event.type == TICK:
            freeParticles.update()


half = WINDOWSIZE/2
tenth = WINDOWSIZE/10

root = Particle((0,0),
                (randint(half-tenth, half+tenth),
                 randint(half-tenth, half+tenth)), screen)
root.stop()

while True:
    input(pygame.event.get())
    pygame.display.flip()
