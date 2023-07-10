cls
gcc -fPIC -o ticks.c.o -c ticks.c
gcc -fPIC -static-libgcc -Wl,--enable-stdcall-fixup -shared -o cppticks.dll ticks.c.o -def ticks.def -lwinmm
strip cppticks.dll

copy cppticks.dll ..
