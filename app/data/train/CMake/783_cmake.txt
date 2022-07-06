find_program(AVR_CC avr-gcc)
find_program(AVR_CXX avr-g++)
find_program(AVR_OBJCOPY avr-objcopy)
find_program(AVR_SIZE_TOOL avr-size)
find_program(AVRDUDE avrdude)

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_C_COMPILER ${AVR_CC})
set(CMAKE_CXX_COMPILER ${AVR_CXX})

SET(AVRDUDE_PROGRAMMER "stk500v1")
SET(AVRDUDE_PORT       "/dev/ttyACM0")
SET(AVRDUDE_MCU        "m328p")

SET(CMCU 		"-mmcu=atmega328p")
SET(CDEFS 		"-DF_CPU=16000000 -D__AVR_ATmega328P__")

SET(CSTANDARD		"-std=gnu99")
SET(CDEBUG		"-gstabs")
SET(CWARN		"-Wall  -Wstrict-prototypes")
SET(CTUNING		"-funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums")

SET(COPT		"-O2")

SET(CFLAGS		"${CMCU} ${CDEFS} ${COPT} ${SDEBUG} ${CWARN} ${CSTANDARD}")
SET(CXXFLAGS		"${CMCU} ${CDEFS} ${COPT}")

SET(CMAKE_C_FLAGS   ${CFLAGS})
SET(CMAKE_CXX_FLAGS ${CXXFLAGS})

add_definitions(${CFLAGS})

function(add_avr TARGET_NAME)
	set(elf_file ${TARGET_NAME})
	set(hex_file ${TARGET_NAME}.hex)

	add_custom_command(
		OUTPUT ${hex_file}
			COMMAND
				${AVR_OBJCOPY} -O ihex -R .eeprom ${elf_file} ${hex_file}
			COMMAND
				${AVR_SIZE_TOOL} ${elf_file}
		DEPENDS ${elf_file}
   	)

	add_custom_target(
		${TARGET_NAME}_hex
		ALL
		DEPENDS ${hex_file}
	)

	add_custom_target(
		upload_${TARGET_NAME}
			${AVRDUDE}
				-c ${AVRDUDE_PROGRAMMER}
				-P ${AVRDUDE_PORT}
				-p ${AVRDUDE_MCU}
				-U flash:w:${hex_file}
				-F # Override invalid signature check.
		DEPENDS ${hex_file}
		COMMENT "Uploading ${hex_file} to ${AVRDUDE_MCU} with ${AVRDUDE_PROGRAMMER}"
	)

endfunction(add_avr)
