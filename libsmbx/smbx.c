#define WIN32_LEAN_AND_MEAN        // Exclude rarely-used stuff from Windows headers

#include <windows.h>
#define KOEXPORT _declspec(dllexport) _stdcall
#define KOSTDCALL _stdcall

#include <string.h>
#include <stdlib.h>
#include <stdio.h>


BOOL _declspec(dllexport) _stdcall DllMain(HANDLE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
    switch(ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
        // MessageBox(NULL, "Attaching...", "Ko", MB_OK);
        break;
    case DLL_PROCESS_DETACH:
        // MessageBox(NULL, "Detatching...", "Ko", MB_OK);
        break;
    }
    return TRUE;
}

void KOEXPORT Class_Initialize()
{
    MessageBox(NULL, "initialized", "Ko", MB_OK);
}

void KOEXPORT Class_Terminate()
{
    MessageBox(NULL, "terminated", "Ko", MB_OK);
}


typedef short VBBool;
#define VBTRUE -1
#define VBFALSE 0

#pragma pack(push, 4)
typedef struct
{
    VBBool Up;
    VBBool Down;
    VBBool Left;
    VBBool Right;
    VBBool Jump;
    VBBool AltJump;
    VBBool Run;
    VBBool AltRun;
    VBBool Drop;
    VBBool Start;
} Controls_t;

typedef struct
{
    double X;
    double Y;
    double Height;
    double Width;
    double SpeedX;
    double SpeedY;
} Location_t;

typedef struct
{
    short DoubleJump;
    VBBool FlySparks;
    VBBool Driving;
    short Quicksand;
    short Bombs;
    VBBool Slippy;
    VBBool Fairy;
    short FairyCD;
    short FairyTime;
    VBBool HasKey;
    short SwordPoke;
    short Hearts;
    VBBool CanFloat;
    VBBool FloatRelease;
    short FloatTime;
    float FloatSpeed;
    short FloatDir;
    short GrabTime; // how long the player has been trying to grab an npc from above
    float GrabSpeed;
    double VineNPC; // the NPC that the player is climbing
    double VineBGO; // the Background (fence) that the player is climbing
    short Wet; // weather or not the player is under water
    VBBool WetFrame; // true if the play should be swimming
    short SwimCount; // cool down between swim strokes
    short NoGravity;
    VBBool Slide; // true if the player is sliding
    VBBool SlideKill; // true if the player is sliding fast enough to kill an NPC
    short Vine; // greater then 0 if the player is climbing
    short NoShellKick; // dont kick a shell
    VBBool ShellSurf; // true if surfing a shell
    short StateNPC;
    short Slope; // the block that the player is standing on when on a slope
    VBBool Stoned; // true of a statue form (tanooki suit)
    short StonedCD; // delay before going back in to stone form
    short StonedTime; // how long the player can remain as a statue
    VBBool SpinJump; // true if spin jumping
    short SpinFrame; // frame for spinning
    short SpinFireDir; // for shooting fireballs while spin jumping
    short Multiplier; // for score increase for multiple hops
    short SlideCounter; // for creating the dust effect when sliding
    short ShowWarp;
    VBBool GroundPound; // for purple yoshi pound
    VBBool GroundPound2; // for purple yoshi pound
    VBBool CanPound; // for purple yoshi pound
    short ForceHold;  // force the player to hold an item for a specific amount of time
    // yoshi powers
    VBBool YoshiYellow;
    VBBool YoshiBlue;
    VBBool YoshiRed;
    short YoshiWingsFrame;
    short YoshiWingsFrameCount;
    // yoshi graphic display
    short YoshiTX;
    short YoshiTY;
    short YoshiTFrame;
    short YoshiTFrameCount;
    short YoshiBX;
    short YoshiBY;
    short YoshiBFrame;
    short YoshiBFrameCount;
    Location_t YoshiTongue;
    float YoshiTongueX;
    short YoshiTongueLength; // length of yoshi's tongue
    VBBool YoshiTonugeBool;
    short YoshiNPC; // the NPC that is in yoshi's mouth
    short YoshiPlayer; // the player that is in yoshi's mouth
    short Dismount; // delay before you can remount
    short NoPlayerCol;
    Location_t Location; // collision detection info
    short Character; // luigi or mario
    Controls_t Controls; // players controls
    short Direction; // the way the player is facing
    short Mount; // 1 for boot, 2 for clown car, 3 for yoshi
    short MountType; // for different types of mounts. blue yoshi, red yoshi, etc
    short MountSpecial;
    short MountOffsetY;
    short MountFrame; // GFX frame for the player's mount
    short State; // 1 for small mario, 2 for super, 3 for fire, 4 for racoon, 5 for tanooki, 6 for hammer
    short Frame;
    float FrameCount;
    short Jump; // how long the player can jump for
    VBBool CanJump; // true if the player can jump
    VBBool CanAltJump; // true if the player can alt jump
    short Effect; // for various effects like shrinking/growing/warping
    double Effect2; // counter for the effects
    VBBool DuckRelease;
    VBBool Duck; // true if ducking
    VBBool DropRelease;
    VBBool StandUp; // aid with collision detection after ducking
    VBBool StandUp2;
    VBBool Bumped; // true if hit by another player
    float Bumped2;
    VBBool Dead; // true if dead
    short TimeToLive; // for returning to the other play after dying
    short Immune; // greater then 0 if immune, this is a counter
    VBBool Immune2; // makes the player blink
    VBBool ForceHitSpot3; // force hitspot 3 for collision detection
    // for getting smashed by a block
    short Pinched1;
    short Pinched2;
    short Pinched3;
    short Pinched4;
    short NPCPinched; // must be > 0 for the player to get crushed
    float m2Speed;
    short HoldingNPC; // What NPC is being held
    VBBool CanGrabNPCs; // If the player can grab NPCs
    short HeldBonus; // the NPC that is in the player's container
    short Section; // What section of the level the player is in
    short WarpCD; // delay before allowing the player to warp again
    short Warp; // the warp the player is using
    short FireBallCD; // How long the player has to wait before he can shoot again
    short FireBallCD2; // How long the player has to wait before he can shoot again
    short TailCount; // Used for the tail swipe
    float RunCount; // To find how long the player has ran for
    VBBool CanFly; // If the player can fly
    VBBool CanFly2;
    short FlyCount; // length of time the player can fly
    VBBool RunRelease; // The player let go of run and pressed again
    VBBool JumpRelease; // The player let go of run and pressed again
    short StandingOnNPC; // The NPC the player is standing on
    short StandingOnTempNPC; // The NPC the player is standing on
    VBBool UnStart; // Player let go of the start button
    float mountBump; // Player hit something while in a mount
    float SpeedFixY;
} Player_t;
#pragma pack(pop)

static Player_t** s_players = NULL;
static size_t s_players_size = 0;
static FILE *s_logFile = NULL;

void KOEXPORT SMBX_Init(void);
void KOEXPORT SMBX_cleanArrays(void);
void KOEXPORT SMBX_Quit(void);
void KOEXPORT SMBX_initPlayers(short playersCount);
void KOEXPORT SMBX_setPlayer(Player_t *player, short playerId);
void KOEXPORT SMBX_printPlayerLocationIntoFile(short playerId);


void KOSTDCALL SMBX_Init(void)
{
    s_logFile = fopen("smbx-log.txt", "a");
}

void KOSTDCALL SMBX_Quit(void)
{
    SMBX_cleanArrays();
    if(s_logFile)
        fclose(s_logFile);
    s_logFile = NULL;
}

void KOSTDCALL SMBX_cleanArrays(void)
{
    if(s_players)
        free(s_players);
    s_players = NULL;
}

void KOSTDCALL SMBX_initPlayers(short playersCount)
{
    s_players = (Player_t**)malloc(sizeof(Player_t**) * (playersCount + 1));
    s_players_size = (size_t)playersCount + 1;
}

void KOEXPORT SMBX_setPlayer(Player_t *player, short playerId)
{
    s_players[playerId] = player;
}

void KOSTDCALL SMBX_printPlayerLocationIntoFile(short playerId)
{
    if(s_logFile)
    {
        fprintf(s_logFile, "player %d pos: x=%g, y=%g\r\n", playerId, s_players[playerId]->Location.X, s_players[playerId]->Location.Y);
        fflush(s_logFile);
    }
}
