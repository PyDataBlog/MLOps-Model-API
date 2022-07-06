# VulpineGE

[reviving this project shortly]

Brand new game engine specifically built for Linux gaming.
It's main design goals are simple:

	- Support Linux natively, with Linux in mind.
	- Support Vulkan.
	- Support a new model format that can speak Eevee/Cycles' shader stuff. What it looks like in Eevee should be roughly what it looks like in Vulpine.
	- Do lighting really well. Try to do whatever we can to make a low poly model look high poly.
	
Some considerations I'll have to make:

	- Full native GlassPanesOS port? (Would prefer not to because branching and all, possibly take the easy way out by linking against Cygwin.)
	- Rename engine to 'FreedomGE' for consistency purposes.
	- Embed LuaJIT into the engine for scripting.
	- Do hybrid raycast/rasterizing?
	- Create our own extensions and plugins for Blender and all.
	- Create texture painting software, cloth stuff, etc. etc.
	- Try to be a serious alternative to the big AAA engines?
	
___

Testing hardware:

	- i7 920 CPU (Overclocked to 4.0Ghz)
	- GTX 980 (proprietary drivers)
	- Debian 9 ("Stretch") (I dual boot this with Arch but don't use proprietary drivers on that)
	- 8GB of DDR3 RAM
	
	- R7 1700X (Overclocked to 4.0Ghz)
	- R9 290X
	- Pop!_OS Alpha
	- 32GB of DDR4 RAM
	
	- i7 3700K (Overclocked to 4.4Ghz)
	- R9 290X
	- Fedora 25
	- 32GB of DDR3 RAM

___

This project currently requires GTK+, which will be removed as a dependency later.
You'll need to dynamically link against it with -ldl and load the module and all that.
For now, I only distribute source code.