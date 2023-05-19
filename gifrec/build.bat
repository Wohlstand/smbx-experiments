cls
gcc -fPIC -DBUILD_DLL -O3 -std=c11 -o gifrec.c.o -c gifrec.c
gcc -fPIC -static-libgcc -Wl,--enable-stdcall-fixup -shared -o gifrec.dll gifrec.c.o -def gifrec.def -lgdi32 -luser32
strip gifrec.dll

copy gifrec.dll ..
