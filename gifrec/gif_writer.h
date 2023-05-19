#ifndef GIF_WRITER_HHHH
#define GIF_WRITER_HHHH

#include <stdio.h>   // for FILE*
#include <stdint.h>  // for integer typedefs

typedef struct GifWriter
{
    FILE* f;
    uint8_t* oldImage;
    bool firstFrame;
    long int delaypos;
} GifWriter;

#endif // GIF_WRITER_HHHH
