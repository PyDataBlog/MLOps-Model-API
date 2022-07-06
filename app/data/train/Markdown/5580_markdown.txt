---
layout: page
title: Projects
permalink: index.html
order: 1
---

---

## Automaton Assault (Team Project of 3 Members)
<div style="text-align: right">Sep 2016 - Jan 2017</div>
Automaton Assault is a 3D top-down shooter game built out of our own 3D engine, Hourglass Engine. The demo is featured with physically-based rendering, dynamic lighting, skinned animation, collision detection, GPU particles, path-finding, behavior tree and Wwise audio system. The engine is also implemented with helpful features such as CPU profiler, debug line/debug text rendering and script to export level from Unity Editor.

#### Personal Contributions
* Direct3D 11 render pipeline
  * Mesh rendering
  * Material and texture manager
  * Compute shader post-processing
  * Dynamic lighting manager and shaders
  * Debug line/text rendering
  * Frustum culling
* Game physics
  * Moving shapes collision detection and response for:
    * AABB to AABB
    * Cylinder to cylinder
    * Cylinder to rotated box 
  * Ray-box intersection with quadtree accelerated
* Event trigger and time based trigger
* CPU profiler
* Script for easy exporting level data from Unity scene
* Wwise audio system integration
  * Integrating sound effects and interective music into game
* Enemy minions design and implementation (Spider bot and turret)

#### Screenshots

<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/AutomatonAssault0.png" target="_blank">
      <img src="/img/AutomatonAssault0_small.jpg" alt="Physically-Based Rendering and dynamic lighting" />
      <p>PBR and dynamic lighting</p>
    </a>
  </div>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/AutomatonAssault1.png" target="_blank">
      <img src="/img/AutomatonAssault1_small.jpg" alt="Post-processing effect: Bloom" />
      <p>Post-processing effect: Bloom</p>
    </a>
  </div>
</p>
<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/AutomatonAssault2.png" target="_blank">
      <img src="/img/AutomatonAssault2_small.jpg" alt="Profiler and debug rendering" />
      <p>Profiler and debug rendering</p>
    </a>
  </div>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/AutomatonAssault3.png" target="_blank">
      <img src="/img/AutomatonAssault3_small.jpg" alt="Quadtree for ray casting" />
      <p>Quadtree for ray casting</p>
    </a>
  </div>
</p>
<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/AutomatonAssault4.png" target="_blank">
      <img src="/img/AutomatonAssault4_small.jpg" alt="Event trigger" />
      <p>Event trigger</p>
    </a>
  </div>
</p>

#### Video

<div style="float: none; clear: both; width: 100%; position: relative; padding-bottom: 56.25%; padding-top: 25px; height: 0;">
    <iframe src="https://www.youtube.com/embed/wFONoH0RR94" frameborder="0" allowfullscreen style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"></iframe>
</div>
*This video demostrates my contributions on dynamic lighting, audio integration, level design and decoration, event battle, creating of turret and spider bot*

<div style="float: none; clear: both; width: 100%; position: relative; padding-bottom: 56.25%; padding-top: 25px; height: 0;">
    <iframe src="https://www.youtube.com/embed/wylUsaEOTbM?start=17" frameborder="0" allowfullscreen style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"></iframe>
</div>
*In this video, I worked on decoration of aisle and interactive music for boss battle*

#### Download

[Download demo executable](https://drive.google.com/open?id=0B4P-ffOPPiTVNUpTRUpDaVhUWW8)

*System requirements:*

* Windows 8.1 or above
* A DirectX 11 compatable device

---
---

## Rhino Engine
<div style="text-align: right">Feb 2016 - May 2016</div>
C++/C# graphics demo project

#### Project Features
* Forward rendering
  * Cascaded shadow map
* Deferred rendering
  * Massive amount of dynamic lights
  * Point light shadow map
  * Screen-space reflection
* Skinned animation with blending
* C# level editor embedding C++ engine
* Multithreaded resource loading
* Lighting with normal map
* Post-processing effects
* Gamma correction

#### Screenshots

<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject0.png" target="_blank">
      <img src="/img/GraphicsProject0_small.jpg" alt="Cascaded shadow map" />
      <p>Cascaded shadow map</p>
    </a>
  </div>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject1.png" target="_blank">
      <img src="/img/GraphicsProject1_small.jpg" alt="Large amount of lights" />
      <p>Large amount of lights</p>
    </a>
  </div>
</p>
<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject2.png" target="_blank">
      <img src="/img/GraphicsProject2_small.jpg" alt="Screen space reflection" />
      <p>Screen space reflection</p>
    </a>
  </div>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject3.png" target="_blank">
      <img src="/img/GraphicsProject3_small.jpg" alt="Fighting game demo" />
      <p>Fighting game demo</p>
    </a>
  </div>
</p>
<p>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject4.png" target="_blank">
      <img src="/img/GraphicsProject4_small.jpg" alt="Refraction shader" />
      Refraction shader
    </a>
    (<a href="/img/GraphicsProject_Anim.gif" target="_blank">gif</a>)
  </div>
  <div style="display: inline-block; max-width: 49%">
    <a href="/img/GraphicsProject5.png" target="_blank">
      <img src="/img/GraphicsProject5_small.jpg" alt="Winform level editor" />
      Winform level editor
    </a>
  </div>
</p>

#### Project Links
[Download demo executable](https://github.com/aosyang/FSGraphicsProject_Binary/releases/download/v0.1.1-alpha/GraphicsEngine_Demos.zip)

[Source Code](https://github.com/aosyang/FSGraphicsProject)

---
---

## Ray Tracer
A ray tracer implemented with Monte Carlo algorithm

#### Screenshot
<div style="display: inline-block; max-width: 100%">
  <a href="/img/RayTracer.png" target="_blank">
    <img src="/img/RayTracer_small.jpg" />
  </a>
</div>

#### Project Links
[Source Code](https://github.com/aosyang/RayTracerWin)

---
---

## Roid Rage (Team Project)
<div style="text-align: right">Oct 2015 - Dec 2015</div>
A 2D top-down space action game made with Unity 3D

#### Personal Contributions
* Prototyped 2D physics-based core gameplay
* Boss battle design and coding
* Visual effect and shader coding (eg. black hole, particle effects)

#### Screenshots

<div style="display: inline-block; max-width: 49%">
  <a href="/img/RoidRage0.png" target="_blank">
    <img src="/img/RoidRage0_small.jpg" />
  </a>
</div>
<div style="display: inline-block; max-width: 49%">
  <a href="/img/RoidRage1.png" target="_blank">
    <img src="/img/RoidRage1_small.jpg" />
  </a>
</div>
<div style="display: inline-block; max-width: 49%">
  <a href="/img/RoidRage2.png" target="_blank">
    <img src="/img/RoidRage2_small.jpg" />
  </a>
</div>
<div style="display: inline-block; max-width: 49%">
  <a href="/img/RoidRage3.png" target="_blank">
    <img src="/img/RoidRage3_small.jpg" />
  </a>
</div>

#### Project Links
[Project page](https://aosyang.itch.io/roid-rage) for webplay and download
