# import libraries
import math
import random
import pygame
from pygame.locals import *

pygame.init()
pygame.mixer.init()
width, height = 800, 600
screen = pygame.display.set_mode((width, height))

keys = [False, False, False, False]
player = [100, 520]

invaders = []
bullets = []
bombs = []
rockets = []
rocketpieces = []

bgimg = pygame.image.load("g:/invaders/paragliding_2017_4_bsl-73.jpg")
invaderimg = pygame.transform.scale(pygame.image.load("g:/invaders/Space-Invaders-PNG-Clipart.png"), (64, 64))
playerimg = pygame.transform.scale(pygame.image.load("g:/invaders/space-invaders-1again.png"), (64, 64))
bulletimg = pygame.transform.scale(pygame.image.load("g:/invaders/square-rounded-512.png"), (16, 16))

# 4 - keep looping through
running = 1
exitcode = 0
invadersmv = 1

# create invaders
for i in range (0, 734, 96):
    for j in range (0, 300, 64):
        invaders.append([i, j])

while running:
    # 5 - clear the screen before drawing it again
    movedown=False
    #screen.fill(0)
    # 6 - draw the screen elements
    screen.blit(bgimg, (0, 0))

    screen.blit(playerimg, player)

    for invader in invaders:
        screen.blit(invaderimg, invader)
    for invader in invaders:
        if invader[0] >= 736:
            invadersmv = -1
            movedown=True
            break
        if invader[0] <= 0:
            invadersmv = 1
            movedown=True
            break
    for invader in invaders:
        invader[0] += invadersmv
        if movedown: invader[1] += 2
    

    for bullet in bullets:
        screen.blit(bulletimg, bullet)
        bullet[1] -= 1
    if len(bullets) > 0 and bullets[0][1] <= -16:
        bullets.pop(0)

    # collision check
    destroyedinvaders = []
    destroyedbullets = []
    for bullet in bullets:
        for invader in invaders:
            if bullet[0] < invader[0] + 16 and bullet[0] + 64 > invader[0] and bullet[1] < invader[1] + 16 and invader[1] + 16 > bullet[1]:
                destroyedbullets.append(bullet)
                destroyedinvaders.append(invader)
                #print('collision')
    bullets = [item for item in bullets if item not in destroyedbullets]
    invaders = [item for item in invaders if item not in destroyedinvaders]
    
    # 9 - Move player
##    if keys[0]:
##        player[1] -= 5
##    elif keys[2]:
##        player[1] += 5
    if keys[1] and player[0] >= 0:
        player[0] -= 5
    elif keys[3] and player[0] <= 736:
        player[0] += 5

    # 7 - update the screen
    pygame.display.flip()

    # 8 - check events
    for event in pygame.event.get():

        if event.type == KEYDOWN:
            if event.key == K_w:
                keys[0] = True
            elif event.key == K_a:
                keys[1] = True
            elif event.key == K_s:
                keys[2] = True
            elif event.key == K_d:
                keys[3] = True
        if event.type == KEYUP:
            if event.key == K_w:
                keys[0] = False
            elif event.key == K_a:
                keys[1] = False
            elif event.key == K_s:
                keys[2] = False
            elif event.key == K_d:
                keys[3] = False

        if event.type == QUIT:
            pygame.quit()
            exit(0)
            
        if event.type == MOUSEBUTTONDOWN:
            #shoot.play()
            if len(bullets) < 3: # up to three bullets
                bullets.append([player[0]+32, player[1]-32])
