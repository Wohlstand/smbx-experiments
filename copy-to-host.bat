@echo off
set remoteroot=Z:\vitaly\.wine\drive_c\smbx1.3
copy smbx-r.exe %remoteroot%
copy comdlg32.ocx %remoteroot%
copy MSWINSCK.ocx %remoteroot%
copy SDL2MixerVB.dll %remoteroot%
copy changelog.txt %remoteroot%
copy README.md %remoteroot%
copy cpprand.dll %remoteroot%
copy gifrec.dll %remoteroot%
copy cppticks.dll %remoteroot%
copy "worlds\the invasion 2\save1.sav" "%remoteroot%\worlds\the invasion 2\save2.sav"
copy screenshots\*.png "%remoteroot%\screenshots-xp"
copy "%remoteroot%\worlds\the invasion 2\save1.sav" "worlds\the invasion 2\save2.sav"
pause
