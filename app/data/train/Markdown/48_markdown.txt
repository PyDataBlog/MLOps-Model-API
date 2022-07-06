---
title: A List of the White Rabbit Mechanics We Are Considering
author: all
date: 15/01/11
tags: [concept, summary, gamemechanics, whiterabbit]
layout: post
---

The white rabbit.
Here is a list of some of the rabbits we could pull out of our designer's magical top hat.

## Cooperative Rope Walking

### Summary

One scenario is while one person crawls across a cable to breach the hull of another ship, the other must move the ship in sync with the other ship to ensure the crawling player doesn't fall.
Could further add elements of wind and obstacles.

### Pros

+ Very explicit cooperative components
+ Leaves room for interesting use of physics
+ Fits well as minigame between core hack and slash

### Cons

+ Direct cooperation between players requires very fast and accurate network syncing
+ Can be frustrating if your partner is very poor at this minigame

## Ghost with Visibility Goggles

### Summary

While one player only sees shimmers of an enemy or an important object, the other can see it very clearly.
They must cooperate 

### Pros

### Cons

## Building Blocks with Imbuement

>  Originally by me but I find the gravitational singularities to be more interesting and probably even easier to implement.  This could be interesting regardless and could be considered in another light where one player can imbue the weaponry of the others. - Calem

### Summary

### Pros

### Cons

## Cooperative Gravitational Singularities

### Summary

Players get a singularity tool that can produce singularities that either push or pull all objects within a sphere of influence.
The map would preferably be prepared to be interactive with this weapon.
For the ice planet, this could be as simple as pre-subdividing the terrain mesh into large icy chunks and into ice shards that can damage enemies.

Interesting physics based puzzles become available.
In the prototype, this was used to traverse a bridge of objects trapped between to singularities.
Another mode is allowing a double jump by jumping onto the top of a singularity, which pushes the character up or allows them to walk on top of the sphere of influence depending on the gravitational factor.

![Little dude traversing a bridge between singularities](http://i.imgur.com/FKQWq0h.png)

![Low framerate gif from proof of concept](http://i.gyazo.com/e228c30c5427656a8d0034d4c99f2f6e.gif)
(note this image has a low framerate as it is a low quality gif :-)  actual prototype is very smooth)

Mesh collision can greatly improve the look and feel, but will dramatically reduce performance.
Smooth performance with mesh collision limited to some 250 cubes.
![Mesh collision rather than box collision](http://i.imgur.com/H5u0owO.png)

A bridge can be formed with 25 sheets if optimising for few objects with convex collision meshes.
![Flatter sheets bridge](http://i.imgur.com/sq5tF9N.png)

### Pros

+ Very easy to implemented if the prototyping is any indication
+ Adds more interesting components to level design
+ Purely physics based cooperation and problem solving, which should be very intuitive, deterministic (like Portal)
+ No particularly special considerations for AI
+ Concept has been tested for resource usage and does well without optimisations
+ Particle effects can be used to hide unrealistic collision a bit and make sense in the context of the singularity
+ Cooperative elements are less contrived, and if friendly fire is enabled accidentally sucking your partner into a singularity could be very amusing given the appropriate sound effects and cues

### Cons

+ Would require level design considerations beyond normal puzzle making (subdivisions)
+ Fine control of singularity location may be awkward (though could be easy if both joysticks used in fine control mode?)
+ Syncing physics of multiple bodies between players may be difficult and needs to be tested.
+ Approximated collision has to be used for the objects in the sphere of influence if there are going to be more than a couple hundred objects

## Transition into spirit with different environmental interactions

### Summary

### Pros

### Cons

## Time travelling with effects on past

### Summary

### Pros

### Cons

## Collector and defender with pinging for resource collection

### Summary

### Pros

### Cons

## Single item to share

### Summary

> The mechanics of this need to be fleshed out more, I think.   - Calem

### Pros

### Cons

## One player can transform the other, changing their abilities

### Summary

### Pros

### Cons

