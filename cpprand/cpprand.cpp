/*
 * TheXTech - A platform game engine ported from old source code for VB6
 *
 * Copyright (c) 2009-2011 Andrew Spinks, original VB6 code
 * Copyright (c) 2020-2021 Vitaly Novichkov <admin@wohlnet.ru>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include <cstdio>
#include <cmath>
#include "pcg/pcg_random.hpp"

#ifdef WIN32
#	define CPPRAND_EXPORT extern "C" __stdcall __declspec(dllexport)
#else
#	define CPPRAND_EXPORT
#endif

//! Main engine
static pcg32 engine;
//! Second engine for isolated non-gameplay randoms
static pcg32 engine_gfx;

CPPRAND_EXPORT void cpprand_seed(int seed)
{
    engine.seed(seed);
}

CPPRAND_EXPORT double cpprand_double()
{
    return ldexp(engine(), -32);
}

CPPRAND_EXPORT int cpprand_int32(int max)
{
    if(max == 0)
    {
        engine();
        return 0;
    }
    return engine() % max;
}

CPPRAND_EXPORT int cpprand_int32N(int max)
{
    if(max == 0)
    {
        engine();
        return 0;
    }
    return engine() % (max + 1);
}

CPPRAND_EXPORT int cpprand_int32_round(int max)
{
    int i;
	
	if(max == 0)
    {
        engine();
        i = 0;
    }
    else 
	{
		i = engine() % ((max + 1) * 2);
	}
	
    if(i == 0)
        return max;

    return i / 2;
}


CPPRAND_EXPORT void cpprand_seed_sec(int seed)
{
    engine_gfx.seed(seed);
}

CPPRAND_EXPORT double cpprand_double_sec()
{
    return ldexp(engine_gfx(), -32);
}

CPPRAND_EXPORT int cpprand_int32_sec(int max)
{
    if(max == 0)
    {
        engine_gfx();
        return 0;
    }
    return engine_gfx() % max;
}

CPPRAND_EXPORT int cpprand_int32N_sec(int max)
{
    if(max == 0)
    {
        engine_gfx();
        return 0;
    }
    return engine_gfx() % (max + 1);
}


CPPRAND_EXPORT int cpprand_int32_round_sec(int max)
{
    int i = cpprand_int32_sec(max * 2);
    if(i == 0)
        return max;
    return i / 2;
}


#ifndef BUILD_DLL
int main(int argc, char** argv)
{
    printf("This is a library and can't be run, but if you want to try, then...\n");
    int seed = -1;

    while(seed == -1)
    {
    	printf("Input a seed: ");
    	scanf("%d", &seed);
    }
    cpprand_seed(seed);

    printf("Your lucky numbers are:");
    for(int i = 0; i < 5; i++)
    {
    	printf(" %d", (int)(cpprand_double() * 50));
    }
    printf("\nGoodbye!\n");

    return 0;
}
#endif
