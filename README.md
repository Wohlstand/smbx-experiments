# smbx-experiments
Source Code for SMBX 1.3

This is research prove ground branch made by Wohlstand, used for the development of [PGE Engine](https://github.com/WohlSoft/PGE-Project). Can be freely used as a ready for work reference for anything.

# How to build and use this
This project requires the "Microsoft Visual Basic 6.0" IDE for a build. You can find it around the Internet on various software historical or abandonware related sites. I have used VB6 SP6 (An SP6 included with an install CD image that I had).
The best environment for a work the Windows XP suggested, however, knowing the fact that Redigit has worked on SMBX with using a Windows Vista.

## The step-by-step how to get it working
* Clone this repository to your end (using any GIT client)
* Download regular **SMBX** 1.3 package: [here](http://wohlsoft.ru/docs/SMBX_1.3.0.1/SMBX%201.3.0.1.zip) or [here](http://wohlsoft.ru/docs/SMBX_Old/SMBX%201.3.0%20Build%2064.7z)
* Download the **config pack** for PGE Project (needed for music and sounds): [here](http://wohlsoft.ru/docs/_laboratory/config_packs/SMBX_13_compatible.zip)
* Open the **SMBX** archive and unpack next folders into the root of this repository: battle, graphics, worlds
* Open the **config pack** archive, open the "SMBX/data" folder and unpack next folders into the root of this repository: music, sound
* Open the **Mario.vbp** project file directly from the repository root, it should be loaded in VB6 IDE. Note that the root of the repository must be a working directory!
* Try to run the game by clicking the run ">" icon. A game should work.
** A tip: please do debug work with a disabled sound, otherwise, VB6 will crash once you'll press the "stop" button.
* If you want to get the .exe file from this, you need to open the "File -> Make smbx-r.exe" menu item.
** Note: while the build process, VB6's window will be stuck, keep patience, it should build the executable in the final.
