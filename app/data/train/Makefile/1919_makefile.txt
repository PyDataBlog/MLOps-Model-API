TARGET	= running.prg

AS	= vasm
ASFLAGS	= -devpac -m68030 -Ftos

$(TARGET): main.s
	$(AS) $(ASFLAGS) -o $@ $<

clean:
	rm -f $(TARGET) *~
