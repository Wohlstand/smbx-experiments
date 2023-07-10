#include <windows.h>
#include <mmsystem.h>
#include <assert.h>

#define TICKS_EXPORT __stdcall __declspec(dllexport)

/* The first (low-resolution) ticks value of the application */
static DWORD start = 0;
static BOOL ticks_started = FALSE;

/* The first high-resolution ticks value of the application */
static LARGE_INTEGER start_ticks;
/* The number of ticks per second of the high-resolution performance counter */
static LARGE_INTEGER ticks_per_second;


static void SDL_SetSystemTimerResolution(const UINT uPeriod)
{
    static UINT timer_period = 0;

    if(uPeriod != timer_period)
    {
        if(timer_period)
            timeEndPeriod(timer_period);

        timer_period = uPeriod;

        if(timer_period)
            timeBeginPeriod(timer_period);
    }
}


TICKS_EXPORT void cppticks_init(void)
{
    BOOL rc;

    if(ticks_started)
        return;

    ticks_started = TRUE;

    SDL_SetSystemTimerResolution(1);

    rc = QueryPerformanceFrequency(&ticks_per_second);
    assert(rc != 0);
    QueryPerformanceCounter(&start_ticks);
}

TICKS_EXPORT void cppticks_quit(void)
{
    SDL_SetSystemTimerResolution(0); /* always release our timer resolution request. */

    start = 0;
    ticks_started = FALSE;
}

TICKS_EXPORT long cppticks_get(void)
{
    LARGE_INTEGER now;
    BOOL rc;

    if(!ticks_started)
        cppticks_init();

    rc = QueryPerformanceCounter(&now);
    assert(rc != 0); /* this should _never_ fail if you're on XP or later. */
    return (long)(((now.QuadPart - start_ticks.QuadPart) * 1000) / ticks_per_second.QuadPart);
}
