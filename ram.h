

#define RAM_BEGIN 0x0000
#define RAM_END 0x1FFF

BYTE ramRead(WORD address);
void ramWrite(WORD address, BYTE value);
void ramLoadRom();
