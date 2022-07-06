# C flags
CFLAGS_scm.o :=$(call as-instr,.arch_extension sec,-DREQUIRES_SEC=1)

# HotPlugs #
# obj-y 					+= hotplug/

obj-$(CONFIG_ALUCARD_HOTPLUG)       += alucard_hotplug.o
obj-$(CONFIG_BLU_PLUG)			    += blu_plug.o
obj-$(CONFIG_BRICKED_HOTPLUG)	    += bricked_hotplug.o
obj-$(CONFIG_FAST_HOTPLUG)		    += fast_hotplug.o
obj-$(CONFIG_INTELLI_HOTPLUG)       += intelli_hotplug.o
obj-$(CONFIG_INTELLI_HOTPLUG)       += intelli_hotplug.o
obj-$(CONFIG_MAKO_HOTPLUG)		    += mako_hotplug.o
obj-$(CONFIG_MSM_HOTPLUG)		    += msm_hotplug.o
obj-$(CONFIG_MSM_SLEEPER)		    += msm_sleeper.o
obj-$(CONFIG_MSM_ZEN_DECISION)	    += msm_zen_decision.o
obj-$(CONFIG_THUNDERPLUG_CONTROL)	+= thunderplug.o
obj-$(CONFIG_MSM_LIMITER)           += msm_limiter.o
