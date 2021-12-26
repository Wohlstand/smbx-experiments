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

static pcg32 engine;

#ifdef WIN32
extern "C" __stdcall __declspec(dllexport)
#endif
void cpprand_seed(int seed)
{
    engine.seed(seed);
}

#ifdef WIN32
extern "C" __stdcall __declspec(dllexport)
#endif
double cpprand_double()
{
    return ldexp(engine(), -32);
}

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
}
