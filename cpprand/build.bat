g++ -fPIC -DBUILD_DLL -O3 -o cpprand.cpp.o -c cpprand.cpp
g++ -fPIC -static-libgcc -static-libstdc++ -Wl,--enable-stdcall-fixup -shared -o cpprand.dll cpprand.cpp.o -def cpprand.def
strip cpprand.dll
