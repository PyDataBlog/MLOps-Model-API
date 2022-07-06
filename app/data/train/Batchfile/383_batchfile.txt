cmd_arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb := arm-poky-linux-gnueabi-gcc -E -Wp,-MD,arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.d.pre.tmp -nostdinc -I/second_extend/yocto_file/linux_yocto/arch/arm/boot/dts -I/second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include -I/second_extend/yocto_file/linux_yocto/drivers/of/testcase-data -undef -D__DTS__ -x assembler-with-cpp -o arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.dts.tmp arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dts ; /second_extend/yocto_file/linux_yocto/scripts/dtc/dtc -O dtb -o arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb -b 0 -i arch/arm/boot/dts/  -d arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.d.dtc.tmp arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.dts.tmp ; cat arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.d.pre.tmp arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.d.dtc.tmp > arch/arm/boot/dts/.imx7d-12x12-lpddr3-arm2-m4.dtb.d

source_arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb := arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dts

deps_arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb := \
  arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2.dts \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/input/input.h \
  arch/arm/boot/dts/imx7d.dtsi \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/clock/imx7d-clock.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/gpio/gpio.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/interrupt-controller/arm-gic.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/interrupt-controller/irq.h \
  arch/arm/boot/dts/imx7d-pinfunc.h \
  arch/arm/boot/dts/skeleton.dtsi \

arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb: $(deps_arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb)

$(deps_arch/arm/boot/dts/imx7d-12x12-lpddr3-arm2-m4.dtb):
