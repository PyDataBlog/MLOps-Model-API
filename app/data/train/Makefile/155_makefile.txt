TARGET	= runmilan.prg

AS	= vasm
ASFLAGS	= -devpac -m68040 -Ftos

$(TARGET): main.s
	$(AS) $(ASFLAGS) -o $@ $<

clean:
	rm -f $(TARGET) *~
