/*
 * TheXTech - A platform game engine ported from old source code for VB6
 *
 * Copyright (c) 2009-2011 Andrew Spinks, original VB6 code
 * Copyright (c) 2020-2023 Vitaly Novichkov <admin@wohlnet.ru>
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

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <windows.h>
#include "gif.h"
#include "gif_writer.h"

#define STB_IMAGE_WRITE_STATIC
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"


#define GIFREC_EXPORT __stdcall __declspec(dllexport)

static bool s_active = false;
static struct GifWriter s_writer = {NULL, NULL, true, false};
static uint32_t    s_delay       = 4;
static uint32_t    s_delayTimer  = 0;
static uint8_t     s_pixels[800 * 600 * 4 + 800];
static const uint32_t c_width = 800;
static const uint32_t c_height = 600;
static char s_log_path[MAX_PATH];
static char s_gif_path[MAX_PATH];
static FILE *s_errorLog = NULL;
static bool s_anyFramesWritten = false;


static void writeErrorMessage(const char *explanation, const char *function, int line)
{
    LPSTR messageBuffer = NULL;
    DWORD errorMessageID = GetLastError();

    if(errorMessageID == 0)
        return; // Nothing

    if(!s_errorLog)
        s_errorLog = fopen(s_log_path, "w");

    if(!s_errorLog)
        return;

    size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                                 NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);

    fprintf(s_errorLog, "%s:%d: %s: %s\r\n", function, line, explanation, messageBuffer);
    fflush(s_errorLog);

    LocalFree(messageBuffer);
}

GIFREC_EXPORT int gifRecord_init(const char *filePath)
{
    if(s_active)
        return -1;

    s_delay = 4;
    s_delayTimer = 0;

    snprintf(s_gif_path, MAX_PATH, "%s", filePath);
    snprintf(s_log_path, MAX_PATH, "%s.log", filePath);

    FILE *gifFile = fopen(s_gif_path, "wb");
    if(!gifFile)
        return -1;

    if(GifBegin(&s_writer, gifFile, c_width, c_height, s_delay))
    {
        s_active = true;
        s_anyFramesWritten = false;
        return 0;
    }

    fclose(gifFile);

    return -1;
}

// Record GIF frame
GIFREC_EXPORT int gifRecord_frame(HDC hd_frame, HBITMAP hd_bitmap)
{
    if(!s_active)
        return -2;

    s_delayTimer += (int)(1000.0 / 65.0);

    if(s_delayTimer >= s_delay * 10)
        s_delayTimer = 0.0;

    if(s_delayTimer != 0.0)
        return 0;

    BITMAPINFO bi;
    memset(&bi, 0, sizeof(BITMAPINFO));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth = c_width;
    bi.bmiHeader.biHeight = -c_height;
    bi.bmiHeader.biPlanes = 1;
    bi.bmiHeader.biBitCount = 32;
    bi.bmiHeader.biCompression = BI_RGB;
    bi.bmiHeader.biSizeImage = c_width * c_height * 4;

    if(GetDIBits(hd_frame, hd_bitmap, 0, c_height, s_pixels, (LPBITMAPINFO)&bi, DIB_RGB_COLORS) == 0)
    {
        writeErrorMessage("GetDIBits failed", __FUNCTION__, __LINE__);
        return -3;
    }

    // Fix the order of pixels
    uint8_t *pix8 = s_pixels;
    for(int i = 0; i < c_width * c_height; ++i)
    {
        uint8_t tmp;
        tmp = pix8[0];
        pix8[0] = pix8[2];
        pix8[2] = tmp;
        pix8 += 4;
    }

    // Write the GIF frame
    GifWriteFrame(&s_writer, s_pixels, c_width, c_height, s_delay);
    s_anyFramesWritten = true;

    return 0;
}

GIFREC_EXPORT void gifRecord_finish()
{
    if(!s_active)
        return;

    GifEnd(&s_writer);
    s_active = false;

    if(s_errorLog)
        fclose(s_errorLog);
    s_errorLog = NULL;

    if(!s_anyFramesWritten)
        DeleteFile(s_gif_path); // If no frames written, just delete the unlucky file
}



// Save a screenshot as PNG
GIFREC_EXPORT int gifRecord_savePng(const char *filePath, HDC hd_frame, HBITMAP hd_bitmap)
{
    BITMAPINFO bi;
    memset(&bi, 0, sizeof(BITMAPINFO));
	bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth = c_width;
    bi.bmiHeader.biHeight = -c_height;
    bi.bmiHeader.biPlanes = 1;
    bi.bmiHeader.biBitCount = 32;
    bi.bmiHeader.biCompression = BI_RGB;
    bi.bmiHeader.biSizeImage = c_width * c_height * 4;

    if(GetDIBits(hd_frame, hd_bitmap, 0, c_height, s_pixels, (LPBITMAPINFO)&bi, DIB_RGB_COLORS) == 0)
    {
        writeErrorMessage("GetDIBits failed", __FUNCTION__, __LINE__);
        return 0;
    }

    // Fix the order of pixels
    uint8_t *pix8 = s_pixels;
    for(int i = 0; i < c_width * c_height; ++i)
    {
        uint8_t tmp;
        tmp = pix8[0];
        pix8[0] = pix8[2];
        pix8[2] = tmp;
        pix8[3] = 0xFF;
        pix8 += 4;
    }

    return stbi_write_png(filePath, c_width, c_height, 4, s_pixels, c_width * 4);
}
