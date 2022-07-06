cmd_arch/arm/boot/dts/imx6sx-17x17-arm2.dtb := arm-poky-linux-gnueabi-gcc -E -Wp,-MD,arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.d.pre.tmp -nostdinc -I/second_extend/yocto_file/linux_yocto/arch/arm/boot/dts -I/second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include -I/second_extend/yocto_file/linux_yocto/drivers/of/testcase-data -undef -D__DTS__ -x assembler-with-cpp -o arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.dts.tmp arch/arm/boot/dts/imx6sx-17x17-arm2.dts ; /second_extend/yocto_file/linux_yocto/scripts/dtc/dtc -O dtb -o arch/arm/boot/dts/imx6sx-17x17-arm2.dtb -b 0 -i arch/arm/boot/dts/  -d arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.d.dtc.tmp arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.dts.tmp ; cat arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.d.pre.tmp arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.d.dtc.tmp > arch/arm/boot/dts/.imx6sx-17x17-arm2.dtb.d

source_arch/arm/boot/dts/imx6sx-17x17-arm2.dtb := arch/arm/boot/dts/imx6sx-17x17-arm2.dts

deps_arch/arm/boot/dts/imx6sx-17x17-arm2.dtb := \
  arch/arm/boot/dts/imx6sx.dtsi \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/clock/imx6sx-clock.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/gpio/gpio.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/interrupt-controller/arm-gic.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/boot/dts/include/dt-bindings/interrupt-controller/irq.h \
  arch/arm/boot/dts/imx6sx-pinfunc.h \
  arch/arm/boot/dts/skeleton.dtsi \

arch/arm/boot/dts/imx6sx-17x17-arm2.dtb: $(deps_arch/arm/boot/dts/imx6sx-17x17-arm2.dtb)

$(deps_arch/arm/boot/dts/imx6sx-17x17-arm2.dtb):
