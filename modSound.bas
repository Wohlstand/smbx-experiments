Attribute VB_Name = "modSound"
Public musicPlaying As Boolean
Public musicLoop As Integer
Public musicName As String

Dim musicX As Long

Public Type MusicEntry
    Alias As String
    Path As String
    Volume As Long
End Type

Public Type SfxEntry
    Alias As String
    Chunk As Long
    Volume As Long
    Chan As Long
End Type

Const list_music_max As Integer = 200
Dim list_music(list_music_max) As MusicEntry
Dim list_music_count As Integer

Const list_sfx_max As Integer = 200
Dim list_sfx(list_sfx_max) As SfxEntry
Dim list_sfx_count As Integer

Public Sub InitMixerX()
    If noSound = True Then Exit Sub
    musicX = 0
    list_music_count = 0
    list_sfx_count = 0
    For i = 0 To list_music_max
        list_music(i).Alias = ""
        list_music(i).Path = ""
    Next
    For i = 0 To list_sfx_max
        list_sfx(i).Alias = ""
        list_sfx(i).Volume = 128
        list_sfx(i).Chan = i
        list_sfx(i).Chunk = 0
    Next
    If SDL_InitAudio < 0 Then         'Initialize SDL Library first
        MsgBox "SDL Error: " & SDL_GetError, vbOKOnly + vbExclamation
        Exit Sub
    End If
    Mix_Init '0                                   'Init SDL Mixer itself
    Mix_OpenAudio 44100, AUDIO_S16LSB, 2, 2048   'Init Open audio stream
    Mix_VolumeMusic 64
    Mix_AllocateChannels 91
End Sub

Public Sub QuitMixerX()
    If noSound = True Then Exit Sub
    Mix_CloseAudio
    Mix_Quit
    SDL_Quit
End Sub

Public Sub AddMusic(Alias As String, Path As String, FallBack As String)
    Dim MusicPathToCheck() As String
    list_music(list_music_count).Alias = Alias
    MusicPathToCheck = Split(Path, "|")
    If FileExists(MusicPathToCheck(0)) Then
        list_music(list_music_count).Path = Path
    Else
        list_music(list_music_count).Path = FallBack
    End If
    list_music(list_music_count).Volume = 128
    list_music_count = list_music_count + 1
End Sub

Public Sub AddSfx(Alias As String, Path As String, FallBack As String)
    Dim PathToLoad As String
    list_sfx(list_sfx_count).Alias = Alias
    If FileExists(Path) Then
        PathToLoad = Path
    Else
        PathToLoad = FallBack
    End If
    DebugMsg "Loading SFX " & PathToLoad & "..."
    list_sfx(list_sfx_count).Chunk = Mix_LoadWAV(PathToLoad)
    If list_sfx(list_sfx_count).Chunk = 0 Then
        DebugMsg "ERROR: SFX " & PathToLoad & " loading error: " & SDL_GetError
    End If
    list_sfx(list_sfx_count).Volume = 128
    list_sfx(list_sfx_count).Chan = list_sfx_count
    list_sfx_count = list_sfx_count + 1
End Sub

Public Sub SetMusicVolume(Alias As String, Volume As Long)
    For i = 0 To list_music_count
        If list_music(i).Alias = Alias Then
            list_music(i).Volume = Volume
        End If
    Next
End Sub

Public Sub SoundPauseAll()
    Mix_PauseMusic
End Sub

Public Sub SoundResumeAll()
    Mix_ResumeMusic
End Sub

Public Sub PlayMusic(Alias As String)
    If musicX <> 0 Then
        Mix_HaltMusic
        Mix_FreeMusic musicX
        musicX = 0
    End If
    For i = 0 To list_music_count
        If list_music(i).Alias = Alias Then
            musicX = Mix_LoadMUS(list_music(i).Path)
            If musicX = 0 Then
                MsgBox "Music " & list_music(i).Path & " opening error: " & SDL_GetError, vbOKOnly + vbExclamation
            Else
                Mix_PlayMusic musicX, -1
                Mix_VolumeMusic list_music(i).Volume
            End If
            Exit For
        End If
    Next
End Sub

Public Sub PlaySfx(Alias As String)
    For i = 0 To list_sfx_count
        If list_sfx(i).Alias = Alias Then
            DebugMsg "Play SFX by alias '" & Alias & "' in channel " & list_sfx(i).Chan
            Mix_PlayChannel list_sfx(i).Chan, list_sfx(i).Chunk, 0
        End If
    Next
End Sub

Public Sub StopSfx(Alias As String)
    For i = 0 To list_sfx_count
        If list_sfx(i).Alias = Alias Then
            Mix_HaltChannel list_sfx(i).Chan
        End If
    Next
End Sub


Public Sub StartMusic(A As Integer) 'play music
    If noSound = True Then Exit Sub
    If (LevelSelect = True Or WorldEditor = True) And LevelEditor = False And GameMenu = False Then 'music on the world map
        StopMusic
        ' mciSendString "play wmusic" & A & " from 10", 0, 0, 0
        PlayMusic ("wmusic" & A)
        musicName = "wmusic" & A
        curWorldMusic = A
    ElseIf A = -1 Then 'P switch music
        StopMusic
        If FreezeNPCs = True Then
            'mciSendString "play stmusic from 10", 0, 0, 0
            PlayMusic ("stmusic")
        Else
            'mciSendString "play smusic from 10", 0, 0, 0
            PlayMusic ("smusic")
        End If
        musicName = "smusic"
        curMusic = -1
    ElseIf PSwitchTime = 0 And PSwitchStop = 0 Then ' level music
        StopMusic
        curMusic = bgMusic(A)
        If bgMusic(A) = 24 Then
            'mciSendString "close music24", 0, 0, 0
            'mciSendString "open " & Chr(34) & FileNamePath & "\" & CustomMusic(A) & Chr(34) & " alias music24", 0, 0, 0
            'mciSendString "setaudio music24 volume to 400", 0, 0, 0
            musicX = Mix_LoadMUS(FileNamePath & "\" & CustomMusic(A))
            Mix_PlayMusic musicX, -1
        Else
            PlayMusic ("music" & bgMusic(A))
        End If
        'mciSendString "play music" & bgMusic(A) & " from 70", 0, 0, 0
        musicName = "music" & bgMusic(A)
    End If
    musicPlaying = True
End Sub

Public Sub StopMusic() 'stop playing music
    If musicPlaying = False Then Exit Sub
    If noSound = True Then Exit Sub
    For A = 1 To 56
        ' If A <= 16 Then mciSendString "stop wmusic" & A, 0, 0, 0
        ' mciSendString "stop music" & A, 0, 0, 0
        If musicX <> 0 Then
            Mix_HaltMusic
            Mix_FreeMusic musicX
            musicX = 0
        End If
    Next A
    'mciSendString "stop smusic", 0, 0, 0
    'mciSendString "stop stmusic", 0, 0, 0
    'mciSendString "stop tmusic", 0, 0, 0
    musicPlaying = False
End Sub

Public Sub PlayInitSound()
    AddSfx "sound29", App.Path & "\sound\do.ogg", App.Path & "\sound\do.mp3"
    PlaySfx "sound29"
End Sub

Public Sub InitSound() 'readys sound and music to be played
    Dim A As Integer
    If noSound = True Then Exit Sub
'SOUNDS
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-jump.mp3" & Chr(34) & " alias sound1", 0, 0, 0
    AddSfx "sound1", App.Path & "\sound\player-jump.ogg", App.Path & "\sound\player-jump.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\stomped.mp3" & Chr(34) & " alias sound2", 0, 0, 0
    AddSfx "sound2", App.Path & "\sound\stomped.ogg", App.Path & "\sound\stomped.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\block-hit.mp3" & Chr(34) & " alias sound3", 0, 0, 0
    AddSfx "sound3", App.Path & "\sound\block-hit.ogg", App.Path & "\sound\block-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\block-smash.mp3" & Chr(34) & " alias sound4", 0, 0, 0
    AddSfx "sound4", App.Path & "\sound\block-smash.ogg", App.Path & "\sound\block-smash.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-shrink.mp3" & Chr(34) & " alias sound5", 0, 0, 0
    AddSfx "sound5", App.Path & "\sound\player-shrink.ogg", App.Path & "\sound\player-shrink.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-grow.mp3" & Chr(34) & " alias sound6", 0, 0, 0
    AddSfx "sound6", App.Path & "\sound\player-grow.ogg", App.Path & "\sound\player-grow.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\mushroom.mp3" & Chr(34) & " alias sound7", 0, 0, 0
    AddSfx "sound7", App.Path & "\sound\mushroom.ogg", App.Path & "\sound\mushroom.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-died.mp3" & Chr(34) & " alias sound8", 0, 0, 0
    AddSfx "sound8", App.Path & "\sound\player-died.ogg", App.Path & "\sound\player-died.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\shell-hit.mp3" & Chr(34) & " alias sound9", 0, 0, 0
    AddSfx "sound9", App.Path & "\sound\shell-hit.ogg", App.Path & "\sound\shell-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-slide.mp3" & Chr(34) & " alias sound10", 0, 0, 0
    AddSfx "sound10", App.Path & "\sound\player-slide.ogg", App.Path & "\sound\player-slide.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\item-dropped.mp3" & Chr(34) & " alias sound11", 0, 0, 0
    AddSfx "sound11", App.Path & "\sound\item-dropped.ogg", App.Path & "\sound\item-dropped.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\has-item.mp3" & Chr(34) & " alias sound12", 0, 0, 0
    AddSfx "sound12", App.Path & "\sound\has-item.ogg", App.Path & "\sound\has-item.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\camera-change.mp3" & Chr(34) & " alias sound13", 0, 0, 0
    AddSfx "sound13", App.Path & "\sound\camera-change.ogg", App.Path & "\sound\camera-change.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\coin.mp3" & Chr(34) & " alias sound14", 0, 0, 0
    AddSfx "sound14", App.Path & "\sound\coin.ogg", App.Path & "\sound\coin.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\1up.mp3" & Chr(34) & " alias sound15", 0, 0, 0
    AddSfx "sound15", App.Path & "\sound\1up.ogg", App.Path & "\sound\1up.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\lava.mp3" & Chr(34) & " alias sound16", 0, 0, 0
    AddSfx "sound16", App.Path & "\sound\lava.ogg", App.Path & "\sound\lava.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\warp.mp3" & Chr(34) & " alias sound17", 0, 0, 0
    AddSfx "sound17", App.Path & "\sound\warp.ogg", App.Path & "\sound\warp.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\fireball.mp3" & Chr(34) & " alias sound18", 0, 0, 0
    AddSfx "sound18", App.Path & "\sound\fireball.ogg", App.Path & "\sound\fireball.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\level-win.mp3" & Chr(34) & " alias sound19", 0, 0, 0
    AddSfx "sound19", App.Path & "\sound\level-win.ogg", App.Path & "\sound\level-win.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\boss-beat.mp3" & Chr(34) & " alias sound20", 0, 0, 0
    AddSfx "sound20", App.Path & "\sound\boss-beat.ogg", App.Path & "\sound\boss-beat.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\sound\dungeon-win.mp3" & Chr(34) & " alias sound21", 0, 0, 0
    AddSfx "sound21", App.Path & "\sound\dungeon-win.ogg", App.Path & "\sound\dungeon-win.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\bullet-bill.mp3" & Chr(34) & " alias sound22", 0, 0, 0
    AddSfx "sound22", App.Path & "\sound\bullet-bill.ogg", App.Path & "\sound\bullet-bill.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\grab.mp3" & Chr(34) & " alias sound23", 0, 0, 0
    AddSfx "sound23", App.Path & "\sound\grab.ogg", App.Path & "\sound\grab.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\spring.mp3" & Chr(34) & " alias sound24", 0, 0, 0
    AddSfx "sound24", App.Path & "\sound\spring.ogg", App.Path & "\sound\spring.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\hammer.mp3" & Chr(34) & " alias sound25", 0, 0, 0
    AddSfx "sound25", App.Path & "\sound\hammer.ogg", App.Path & "\sound\hammer.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\slide.mp3" & Chr(34) & " alias sound26", 0, 0, 0
    AddSfx "sound26", App.Path & "\sound\slide.ogg", App.Path & "\sound\slide.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\newpath.mp3" & Chr(34) & " alias sound27", 0, 0, 0
    AddSfx "sound27", App.Path & "\sound\newpath.ogg", App.Path & "\sound\newpath.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\level-select.mp3" & Chr(34) & " alias sound28", 0, 0, 0
    AddSfx "sound28", App.Path & "\sound\level-select.ogg", App.Path & "\sound\level-select.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\do.mp3" & Chr(34) & " alias sound29", 0, 0, 0
    ' AddSfx "sound29", App.path & "\sound\do.ogg", App.path & "\sound\do.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\pause.mp3" & Chr(34) & " alias sound30", 0, 0, 0
    AddSfx "sound30", App.Path & "\sound\pause.ogg", App.Path & "\sound\pause.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\key.mp3" & Chr(34) & " alias sound31", 0, 0, 0
    AddSfx "sound31", App.Path & "\sound\key.ogg", App.Path & "\sound\key.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\pswitch.mp3" & Chr(34) & " alias sound32", 0, 0, 0
    AddSfx "sound32", App.Path & "\sound\pswitch.ogg", App.Path & "\sound\pswitch.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\tail.mp3" & Chr(34) & " alias sound33", 0, 0, 0
    AddSfx "sound33", App.Path & "\sound\tail.ogg", App.Path & "\sound\tail.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\racoon.mp3" & Chr(34) & " alias sound34", 0, 0, 0
    AddSfx "sound34", App.Path & "\sound\racoon.ogg", App.Path & "\sound\racoon.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\boot.mp3" & Chr(34) & " alias sound35", 0, 0, 0
    AddSfx "sound35", App.Path & "\sound\boot.ogg", App.Path & "\sound\boot.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smash.mp3" & Chr(34) & " alias sound36", 0, 0, 0
    AddSfx "sound36", App.Path & "\sound\smash.ogg", App.Path & "\sound\smash.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\thwomp.mp3" & Chr(34) & " alias sound37", 0, 0, 0
    AddSfx "sound37", App.Path & "\sound\thwomp.ogg", App.Path & "\sound\thwomp.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\birdo-spit.mp3" & Chr(34) & " alias sound38", 0, 0, 0
    AddSfx "sound38", App.Path & "\sound\birdo-spit.ogg", App.Path & "\sound\birdo-spit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\birdo-hit.mp3" & Chr(34) & " alias sound39", 0, 0, 0
    AddSfx "sound39", App.Path & "\sound\birdo-hit.ogg", App.Path & "\sound\birdo-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smb2-exit.mp3" & Chr(34) & " alias sound40", 0, 0, 0
    AddSfx "sound40", App.Path & "\sound\smb2-exit.ogg", App.Path & "\sound\smb2-exit.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\sound\birdo-beat.mp3" & Chr(34) & " alias sound41", 0, 0, 0
    AddSfx "sound41", App.Path & "\sound\birdo-beat.ogg", App.Path & "\sound\birdo-beat.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\npc-fireball.mp3" & Chr(34) & " alias sound42", 0, 0, 0
    AddSfx "sound42", App.Path & "\sound\npc-fireball.ogg", App.Path & "\sound\npc-fireball.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\fireworks.mp3" & Chr(34) & " alias sound43", 0, 0, 0
    AddSfx "sound43", App.Path & "\sound\fireworks.ogg", App.Path & "\sound\fireworks.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\bowser-killed.mp3" & Chr(34) & " alias sound44", 0, 0, 0
    AddSfx "sound44", App.Path & "\sound\bowser-killed.ogg", App.Path & "\sound\bowser-killed.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\game-beat.mp3" & Chr(34) & " alias sound45", 0, 0, 0
    AddSfx "sound45", App.Path & "\sound\game-beat.ogg", App.Path & "\sound\game-beat.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\door.mp3" & Chr(34) & " alias sound46", 0, 0, 0
    AddSfx "sound46", App.Path & "\sound\door.ogg", App.Path & "\sound\door.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\message.mp3" & Chr(34) & " alias sound47", 0, 0, 0
    AddSfx "sound47", App.Path & "\sound\message.ogg", App.Path & "\sound\message.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\yoshi.mp3" & Chr(34) & " alias sound48", 0, 0, 0
    AddSfx "sound48", App.Path & "\sound\yoshi.ogg", App.Path & "\sound\yoshi.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\yoshi-hurt.mp3" & Chr(34) & " alias sound49", 0, 0, 0
    AddSfx "sound49", App.Path & "\sound\yoshi-hurt.ogg", App.Path & "\sound\yoshi-hurt.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\yoshi-tongue.mp3" & Chr(34) & " alias sound50", 0, 0, 0
    AddSfx "sound50", App.Path & "\sound\yoshi-tongue.ogg", App.Path & "\sound\yoshi-tongue.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\yoshi-egg.mp3" & Chr(34) & " alias sound51", 0, 0, 0
    AddSfx "sound51", App.Path & "\sound\yoshi-egg.ogg", App.Path & "\sound\yoshi-egg.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\got-star.mp3" & Chr(34) & " alias sound52", 0, 0, 0
    AddSfx "sound52", App.Path & "\sound\got-star.ogg", App.Path & "\sound\got-star.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-kill.mp3" & Chr(34) & " alias sound53", 0, 0, 0
    AddSfx "sound53", App.Path & "\sound\zelda-kill.ogg", App.Path & "\sound\zelda-kill.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\player-died2.mp3" & Chr(34) & " alias sound54", 0, 0, 0
    AddSfx "sound54", App.Path & "\sound\player-died2.ogg", App.Path & "\sound\player-died2.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\yoshi-swallow.mp3" & Chr(34) & " alias sound55", 0, 0, 0
    AddSfx "sound55", App.Path & "\sound\yoshi-swallow.ogg", App.Path & "\sound\yoshi-swallow.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\ring.mp3" & Chr(34) & " alias sound56", 0, 0, 0
    AddSfx "sound56", App.Path & "\sound\ring.ogg", App.Path & "\sound\ring.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\dry-bones.mp3" & Chr(34) & " alias sound57", 0, 0, 0
    AddSfx "sound57", App.Path & "\sound\dry-bones.ogg", App.Path & "\sound\dry-bones.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smw-checkpoint.mp3" & Chr(34) & " alias sound58", 0, 0, 0
    AddSfx "sound58", App.Path & "\sound\smw-checkpoint.ogg", App.Path & "\sound\smw-checkpoint.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\dragon-coin.mp3" & Chr(34) & " alias sound59", 0, 0, 0
    AddSfx "sound59", App.Path & "\sound\dragon-coin.ogg", App.Path & "\sound\dragon-coin.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smw-exit.mp3" & Chr(34) & " alias sound60", 0, 0, 0
    AddSfx "sound60", App.Path & "\sound\smw-exit.ogg", App.Path & "\sound\smw-exit.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smw-blaarg.mp3" & Chr(34) & " alias sound61", 0, 0, 0
    AddSfx "sound61", App.Path & "\sound\smw-blaarg.ogg", App.Path & "\sound\smw-blaarg.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\wart-bubble.mp3" & Chr(34) & " alias sound62", 0, 0, 0
    AddSfx "sound62", App.Path & "\sound\wart-bubble.ogg", App.Path & "\sound\wart-bubble.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\wart-die.mp3" & Chr(34) & " alias sound63", 0, 0, 0
    AddSfx "sound63", App.Path & "\sound\wart-die.ogg", App.Path & "\sound\wart-die.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-block-hit.mp3" & Chr(34) & " alias sound64", 0, 0, 0
    AddSfx "sound64", App.Path & "\sound\sm-block-hit.ogg", App.Path & "\sound\sm-block-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-killed.mp3" & Chr(34) & " alias sound65", 0, 0, 0
    AddSfx "sound65", App.Path & "\sound\sm-killed.ogg", App.Path & "\sound\sm-killed.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-hurt.mp3" & Chr(34) & " alias sound66", 0, 0, 0
    AddSfx "sound66", App.Path & "\sound\sm-hurt.ogg", App.Path & "\sound\sm-hurt.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-glass.mp3" & Chr(34) & " alias sound67", 0, 0, 0
    AddSfx "sound67", App.Path & "\sound\sm-glass.ogg", App.Path & "\sound\sm-glass.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-boss-hit.mp3" & Chr(34) & " alias sound68", 0, 0, 0
    AddSfx "sound68", App.Path & "\sound\sm-boss-hit.ogg", App.Path & "\sound\sm-boss-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-cry.mp3" & Chr(34) & " alias sound69", 0, 0, 0
    AddSfx "sound69", App.Path & "\sound\sm-cry.ogg", App.Path & "\sound\sm-cry.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\sm-explosion.mp3" & Chr(34) & " alias sound70", 0, 0, 0
    AddSfx "sound70", App.Path & "\sound\sm-explosion.ogg", App.Path & "\sound\sm-explosion.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\climbing.mp3" & Chr(34) & " alias sound71", 0, 0, 0
    AddSfx "sound71", App.Path & "\sound\climbing.ogg", App.Path & "\sound\climbing.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\swim.mp3" & Chr(34) & " alias sound72", 0, 0, 0
    AddSfx "sound72", App.Path & "\sound\swim.ogg", App.Path & "\sound\swim.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\grab2.mp3" & Chr(34) & " alias sound73", 0, 0, 0
    AddSfx "sound73", App.Path & "\sound\grab2.ogg", App.Path & "\sound\grab2.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smw-saw.mp3" & Chr(34) & " alias sound74", 0, 0, 0
    AddSfx "sound74", App.Path & "\sound\smw-saw.ogg", App.Path & "\sound\smw-saw.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smb2-throw.mp3" & Chr(34) & " alias sound75", 0, 0, 0
    AddSfx "sound75", App.Path & "\sound\smb2-throw.ogg", App.Path & "\sound\smb2-throw.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\smb2-hit.mp3" & Chr(34) & " alias sound76", 0, 0, 0
    AddSfx "sound76", App.Path & "\sound\smb2-hit.ogg", App.Path & "\sound\smb2-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-stab.mp3" & Chr(34) & " alias sound77", 0, 0, 0
    AddSfx "sound77", App.Path & "\sound\zelda-stab.ogg", App.Path & "\sound\zelda-stab.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-hurt.mp3" & Chr(34) & " alias sound78", 0, 0, 0
    AddSfx "sound78", App.Path & "\sound\zelda-hurt.ogg", App.Path & "\sound\zelda-hurt.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-heart.mp3" & Chr(34) & " alias sound79", 0, 0, 0
    AddSfx "sound79", App.Path & "\sound\zelda-heart.ogg", App.Path & "\sound\zelda-heart.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-died.mp3" & Chr(34) & " alias sound80", 0, 0, 0
    AddSfx "sound80", App.Path & "\sound\zelda-died.ogg", App.Path & "\sound\zelda-died.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-rupee.mp3" & Chr(34) & " alias sound81", 0, 0, 0
    AddSfx "sound81", App.Path & "\sound\zelda-rupee.ogg", App.Path & "\sound\zelda-rupee.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-fire.mp3" & Chr(34) & " alias sound82", 0, 0, 0
    AddSfx "sound82", App.Path & "\sound\zelda-fire.ogg", App.Path & "\sound\zelda-fire.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-item.mp3" & Chr(34) & " alias sound83", 0, 0, 0
    AddSfx "sound83", App.Path & "\sound\zelda-item.ogg", App.Path & "\sound\zelda-item.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-key.mp3" & Chr(34) & " alias sound84", 0, 0, 0
    AddSfx "sound84", App.Path & "\sound\zelda-key.ogg", App.Path & "\sound\zelda-key.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-shield.mp3" & Chr(34) & " alias sound85", 0, 0, 0
    AddSfx "sound85", App.Path & "\sound\zelda-shield.ogg", App.Path & "\sound\zelda-shield.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-dash.mp3" & Chr(34) & " alias sound86", 0, 0, 0
    AddSfx "sound86", App.Path & "\sound\zelda-dash.ogg", App.Path & "\sound\zelda-dash.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-fairy.mp3" & Chr(34) & " alias sound87", 0, 0, 0
    AddSfx "sound87", App.Path & "\sound\zelda-fairy.ogg", App.Path & "\sound\zelda-fairy.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-grass.mp3" & Chr(34) & " alias sound88", 0, 0, 0
    AddSfx "sound88", App.Path & "\sound\zelda-grass.ogg", App.Path & "\sound\zelda-grass.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-hit.mp3" & Chr(34) & " alias sound89", 0, 0, 0
    AddSfx "sound89", App.Path & "\sound\zelda-hit.ogg", App.Path & "\sound\zelda-hit.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\zelda-sword-beam.mp3" & Chr(34) & " alias sound90", 0, 0, 0
    AddSfx "sound90", App.Path & "\sound\zelda-sword-beam.ogg", App.Path & "\sound\zelda-sword-beam.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\sound\bubble.mp3" & Chr(34) & " alias sound91", 0, 0, 0
    AddSfx "sound91", App.Path & "\sound\bubble.ogg", App.Path & "\sound\bubble.mp3"
    modLoadGFX.UpdateLoad
'WORLD / MISC MUSIC
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world1.mp3" & Chr(34) & " alias wmusic1", 0, 0, 0
    AddMusic "wmusic1", App.Path & "\music\smb3-world1.spc", App.Path & "\music\smb3-world1.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world4.mp3" & Chr(34) & " alias wmusic2", 0, 0, 0
    AddMusic "wmusic2", App.Path & "\music\smb3-world4.spc", App.Path & "\music\smb3-world4.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world7.mp3" & Chr(34) & " alias wmusic3", 0, 0, 0
    AddMusic "wmusic3", App.Path & "\music\smb3-world7.spc", App.Path & "\music\smb3-world7.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-worldmap.mp3" & Chr(34) & " alias wmusic4", 0, 0, 0
    AddMusic "wmusic4", App.Path & "\music\smw-worldmap.spc", App.Path & "\music\smw-worldmap.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\nsmb-world.mp3" & Chr(34) & " alias wmusic5", 0, 0, 0
    AddMusic "wmusic5", App.Path & "\music\nsmb-world.ogg", App.Path & "\music\nsmb-world.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world2.mp3" & Chr(34) & " alias wmusic6", 0, 0, 0
    AddMusic "wmusic6", App.Path & "\music\smb3-world2.spc", App.Path & "\music\smb3-world2.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-forestofillusion.mp3" & Chr(34) & " alias wmusic7", 0, 0, 0
    AddMusic "wmusic7", App.Path & "\music\smw-forestofillusion.spc", App.Path & "\music\smw-forestofillusion.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world3.mp3" & Chr(34) & " alias wmusic8", 0, 0, 0
    AddMusic "wmusic8", App.Path & "\music\smb3-world3.spc", App.Path & "\music\smb3-world3.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world8.mp3" & Chr(34) & " alias wmusic9", 0, 0, 0
    AddMusic "wmusic9", App.Path & "\music\smb3-world8.spc", App.Path & "\music\smb3-world8.mp3"
    modLoadGFX.UpdateLoad
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world6.mp3" & Chr(34) & " alias wmusic10", 0, 0, 0
    AddMusic "wmusic10", App.Path & "\music\smb3-world6.spc", App.Path & "\music\smb3-world6.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-world5.mp3" & Chr(34) & " alias wmusic11", 0, 0, 0
    AddMusic "wmusic11", App.Path & "\music\smb3-world5.spc", App.Path & "\music\smb3-world5.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-special.mp3" & Chr(34) & " alias wmusic12", 0, 0, 0
    AddMusic "wmusic12", App.Path & "\music\smw-special.spc", App.Path & "\music\smw-special.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-bowserscastle.mp3" & Chr(34) & " alias wmusic13", 0, 0, 0
    AddMusic "wmusic13", App.Path & "\music\smw-bowserscastle.spc", App.Path & "\music\smw-bowserscastle.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-starroad.mp3" & Chr(34) & " alias wmusic14", 0, 0, 0
    AddMusic "wmusic14", App.Path & "\music\smw-starroad.spc", App.Path & "\music\smw-starroad.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-yoshisisland.mp3" & Chr(34) & " alias wmusic15", 0, 0, 0
    AddMusic "wmusic15", App.Path & "\music\smw-yoshisisland.spc", App.Path & "\music\smw-yoshisisland.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-vanilladome.mp3" & Chr(34) & " alias wmusic16", 0, 0, 0
    AddMusic "wmusic16", App.Path & "\music\smw-vanilladome.spc", App.Path & "\music\smw-vanilladome.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smw-switch.mp3" & Chr(34) & " alias smusic", 0, 0, 0 'p switch music
    AddMusic "smusic", App.Path & "\music\smw-switch.spc", App.Path & "\music\smw-switch.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smb3-switch.mp3" & Chr(34) & " alias stmusic", 0, 0, 0 'p switch music
    AddMusic "stmusic", App.Path & "\music\smb3-switch.spc", App.Path & "\music\smb3-switch.mp3"
    'mciSendString "open " & Chr(34) & App.path & "\music\smg-title.mp3" & Chr(34) & " alias tmusic", 0, 0, 0 'credit music
    AddMusic "tmusic", App.Path & "\music\smg-title.ogg", App.Path & "\music\smg-title.mp3"
    
'LEVEL MUSIC
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-overworld.mp3" & Chr(34) & " alias music1", 0, 0, 0
    AddMusic "music1", App.Path & "\music\smb3-overworld.spc", App.Path & "\music\smb3-overworld.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-sky.mp3" & Chr(34) & " alias music2", 0, 0, 0
    AddMusic "music2", App.Path & "\music\smb3-sky.spc", App.Path & "\music\smb3-sky.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-castle.mp3" & Chr(34) & " alias music3", 0, 0, 0
    AddMusic "music3", App.Path & "\music\smb3-castle.spc", App.Path & "\music\smb3-castle.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-underground.mp3" & Chr(34) & " alias music4", 0, 0, 0
    AddMusic "music4", App.Path & "\music\smb3-underground.spc", App.Path & "\music\smb3-underground.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb2-overworld.mp3" & Chr(34) & " alias music5", 0, 0, 0
    AddMusic "music5", App.Path & "\music\smb2-overworld.spc", App.Path & "\music\smb2-overworld.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-boss.mp3" & Chr(34) & " alias music6", 0, 0, 0
    AddMusic "music6", App.Path & "\music\smb3-boss.spc", App.Path & "\music\smb3-boss.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb-underground.mp3" & Chr(34) & " alias music7", 0, 0, 0
    AddMusic "music7", App.Path & "\music\super-mario-bros-nes-NSF-ID2098.nsf|1", App.Path & "\music\smb-underground.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sf-corneria.mp3" & Chr(34) & " alias music8", 0, 0, 0
    AddMusic "music8", App.Path & "\music\sf-corneria.spc", App.Path & "\music\sf-corneria.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb-overworld.mp3" & Chr(34) & " alias music9", 0, 0, 0
    AddMusic "music9", App.Path & "\music\smb-overworld.spc", App.Path & "\music\smb-overworld.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-overworld.mp3" & Chr(34) & " alias music10", 0, 0, 0
    AddMusic "music10", App.Path & "\music\smw-overworld.spc", App.Path & "\music\smw-overworld.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sm-brinstar.mp3" & Chr(34) & " alias music11", 0, 0, 0
    AddMusic "music11", App.Path & "\music\sm-brinstar.spc", App.Path & "\music\sm-brinstar.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sm-crateria.mp3" & Chr(34) & " alias music12", 0, 0, 0
    AddMusic "music12", App.Path & "\music\sm-crateria.spc", App.Path & "\music\sm-crateria.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\nsmb-overworld.mp3" & Chr(34) & " alias music13", 0, 0, 0
    AddMusic "music13", App.Path & "\music\nsmb-overworld.ogg", App.Path & "\music\nsmb-overworld.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sm64-desert.mp3" & Chr(34) & " alias music14", 0, 0, 0
    AddMusic "music14", App.Path & "\music\sm64-desert.ogg", App.Path & "\music\sm64-desert.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb2-boss.mp3" & Chr(34) & " alias music15", 0, 0, 0
    AddMusic "music15", App.Path & "\music\smb2-boss.spc", App.Path & "\music\smb2-boss.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-forestmaze.mp3" & Chr(34) & " alias music16", 0, 0, 0
    AddMusic "music16", App.Path & "\music\mariorpg-forestmaze.spc", App.Path & "\music\mariorpg-forestmaze.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-ghosthouse.mp3" & Chr(34) & " alias music17", 0, 0, 0
    AddMusic "music17", App.Path & "\music\smw-ghosthouse.spc", App.Path & "\music\smw-ghosthouse.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smg-beach-bowl-galaxy.mp3" & Chr(34) & " alias music18", 0, 0, 0
    AddMusic "music18", App.Path & "\music\smg-beach-bowl-galaxy.ogg", App.Path & "\music\smg-beach-bowl-galaxy.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\ssbb-airship.mp3" & Chr(34) & " alias music19", 0, 0, 0
    AddMusic "music19", App.Path & "\music\ssbb-airship.ogg", App.Path & "\music\ssbb-airship.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smg-star-reactor.mp3" & Chr(34) & " alias music20", 0, 0, 0
    AddMusic "music20", App.Path & "\music\smg-star-reactor.ogg", App.Path & "\music\smg-star-reactor.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-bowser.mp3" & Chr(34) & " alias music21", 0, 0, 0
    AddMusic "music21", App.Path & "\music\mariorpg-bowser.spc", App.Path & "\music\mariorpg-bowser.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\tds-metroid-charge.mp3" & Chr(34) & " alias music22", 0, 0, 0
    AddMusic "music22", App.Path & "\music\tds-metroid-charge.ogg", App.Path & "\music\tds-metroid-charge.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\z3-lost-woods.mp3" & Chr(34) & " alias music23", 0, 0, 0
    AddMusic "music23", App.Path & "\music\z3-lost-woods.spc", App.Path & "\music\z3-lost-woods.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb2-underground.mp3" & Chr(34) & " alias music25", 0, 0, 0
    AddMusic "music25", App.Path & "\music\smb2-underground.spc", App.Path & "\music\smb2-underground.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-castle.mp3" & Chr(34) & " alias music26", 0, 0, 0
    AddMusic "music26", App.Path & "\music\mario64-castle.ogg", App.Path & "\music\mario64-castle.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-maintheme.mp3" & Chr(34) & " alias music27", 0, 0, 0
    AddMusic "music27", App.Path & "\music\mario64-maintheme.ogg", App.Path & "\music\mario64-maintheme.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-sky.mp3" & Chr(34) & " alias music28", 0, 0, 0
    AddMusic "music28", App.Path & "\music\smw-sky.spc", App.Path & "\music\smw-sky.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-cave.mp3" & Chr(34) & " alias music29", 0, 0, 0
    AddMusic "music29", App.Path & "\music\smw-cave.spc", App.Path & "\music\smw-cave.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-mariospad.mp3" & Chr(34) & " alias music30", 0, 0, 0
    AddMusic "music30", App.Path & "\music\mariorpg-mariospad.spc", App.Path & "\music\mariorpg-mariospad.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-seasidetown.mp3" & Chr(34) & " alias music31", 0, 0, 0
    AddMusic "music31", App.Path & "\music\mariorpg-seasidetown.spc", App.Path & "\music\mariorpg-seasidetown.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-tadpolepond.mp3" & Chr(34) & " alias music32", 0, 0, 0
    AddMusic "music32", App.Path & "\music\mariorpg-tadpolepond.spc", App.Path & "\music\mariorpg-tadpolepond.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-nimbusland.mp3" & Chr(34) & " alias music33", 0, 0, 0
    AddMusic "music33", App.Path & "\music\mariorpg-nimbusland.spc", App.Path & "\music\mariorpg-nimbusland.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mariorpg-rosetown.mp3" & Chr(34) & " alias music34", 0, 0, 0
    AddMusic "music34", App.Path & "\music\mariorpg-rosetown.spc", App.Path & "\music\mariorpg-rosetown.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-snowmountain.mp3" & Chr(34) & " alias music35", 0, 0, 0
    AddMusic "music35", App.Path & "\music\mario64-snowmountain.ogg", App.Path & "\music\mario64-snowmountain.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-boss.mp3" & Chr(34) & " alias music36", 0, 0, 0
    AddMusic "music36", App.Path & "\music\mario64-boss.ogg", App.Path & "\music\mario64-boss.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\pm-shiver-mountain.mp3" & Chr(34) & " alias music37", 0, 0, 0
    AddMusic "music37", App.Path & "\music\pm-shiver-mountain.ogg", App.Path & "\music\pm-shiver-mountain.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\pm-yoshis-village.mp3" & Chr(34) & " alias music38", 0, 0, 0
    AddMusic "music38", App.Path & "\music\pm-yoshis-village.ogg", App.Path & "\music\pm-yoshis-village.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\ssbb-zelda2.mp3" & Chr(34) & " alias music39", 0, 0, 0
    AddMusic "music39", App.Path & "\music\ssbb-zelda2.ogg", App.Path & "\music\ssbb-zelda2.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\ssbb-meta.mp3" & Chr(34) & " alias music40", 0, 0, 0
    AddMusic "music40", App.Path & "\music\ssbb-meta.ogg", App.Path & "\music\ssbb-meta.mp3"
    modLoadGFX.UpdateLoad
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-castle.mp3" & Chr(34) & " alias music41", 0, 0, 0
    AddMusic "music41", App.Path & "\music\smw-castle.spc", App.Path & "\music\smw-castle.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb-castle.mp3" & Chr(34) & " alias music42", 0, 0, 0
    AddMusic "music42", App.Path & "\music\smb-castle.spc", App.Path & "\music\smb-castle.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb2-wart.mp3" & Chr(34) & " alias music43", 0, 0, 0
    AddMusic "music43", App.Path & "\music\smb2-wart.spc", App.Path & "\music\smb2-wart.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sm-itemroom.mp3" & Chr(34) & " alias music44", 0, 0, 0
    AddMusic "music44", App.Path & "\music\sm-itemroom.spc", App.Path & "\music\sm-itemroom.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\sm-brain.mp3" & Chr(34) & " alias music45", 0, 0, 0
    AddMusic "music45", App.Path & "\music\sm-brain.spc", App.Path & "\music\sm-brain.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb-water.mp3" & Chr(34) & " alias music46", 0, 0, 0
    AddMusic "music46", App.Path & "\music\smb-water.spc", App.Path & "\music\smb-water.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-water.mp3" & Chr(34) & " alias music47", 0, 0, 0
    AddMusic "music47", App.Path & "\music\smb3-water.spc", App.Path & "\music\smb3-water.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-water.mp3" & Chr(34) & " alias music48", 0, 0, 0
    AddMusic "music48", App.Path & "\music\smw-water.spc", App.Path & "\music\smw-water.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-water.mp3" & Chr(34) & " alias music49", 0, 0, 0
    AddMusic "music49", App.Path & "\music\mario64-water.ogg", App.Path & "\music\mario64-water.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mario64-cave.mp3" & Chr(34) & " alias music50", 0, 0, 0
    AddMusic "music50", App.Path & "\music\mario64-cave.ogg", App.Path & "\music\mario64-cave.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smw-boss.mp3" & Chr(34) & " alias music51", 0, 0, 0
    AddMusic "music51", App.Path & "\music\smw-boss.spc", App.Path & "\music\smw-boss.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\ssbb-underground.mp3" & Chr(34) & " alias music52", 0, 0, 0
    AddMusic "music52", App.Path & "\music\ssbb-underground.ogg", App.Path & "\music\ssbb-underground.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\ssbb-waluigi.mp3" & Chr(34) & " alias music53", 0, 0, 0
    AddMusic "music53", App.Path & "\music\ssbb-waluigi.ogg", App.Path & "\music\ssbb-waluigi.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smb3-hammer.mp3" & Chr(34) & " alias music54", 0, 0, 0
    AddMusic "music54", App.Path & "\music\smb3-hammer.spc", App.Path & "\music\smb3-hammer.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\smg2-fg.mp3" & Chr(34) & " alias music55", 0, 0, 0
    AddMusic "music55", App.Path & "\music\smg2-fg.ogg", App.Path & "\music\smg2-fg.mp3"
    ' mciSendString "open " & Chr(34) & App.path & "\music\mkwii-mushroom-gorge.mp3" & Chr(34) & " alias music56", 0, 0, 0
    AddMusic "music56", App.Path & "\music\mkwii-mushroom-gorge.ogg", App.Path & "\music\mkwii-mushroom-gorge.mp3"
    modLoadGFX.UpdateLoad
    For A = 1 To 56 'set volume
        'If A <= 16 Then mciSendString "setaudio wmusic" & A & " volume to 500", 0, 0, 0 'world map music is a little louder
        If A <= 16 Then SetMusicVolume "wmusic" & A, 64
        'mciSendString "setaudio music" & A & " volume to 400", 0, 0, 0
        SetMusicVolume "music" & A, 52
    Next A
    'mciSendString "setaudio smusic volume to 500", 0, 0, 0
    SetMusicVolume "smusic", 64
    'mciSendString "setaudio stmusic volume to 500", 0, 0, 0
    SetMusicVolume "stmusic", 64
    'mciSendString "setaudio tmusic volume to 500", 0, 0, 0
    SetMusicVolume "tmusic", 64
End Sub

Public Sub PlaySound(A As Integer) 'play a sound
    Dim soundString As String
    If noSound = True Then Exit Sub
    If (GameMenu = False Or A = 26 Or A = 29) And GameOutro = False Then
        If numPlayers > 2 And nPlay.Online = False Then SoundPause(10) = 1
        If SoundPause(A) = 0 Then 'if the sound wasn't just played
            ' soundString = "stop sound" & A
            ' mciSendString soundString, 0, 0, 0
            StopSfx "sound" & A
            ' soundString = "play sound" & A & " from 10"
            ' mciSendString soundString, 0, 0, 0
            PlaySfx "sound" & A
            DebugMsg "Play sound 'sound" & A & "'"

'set the delay before a sound can be played again
            If A = 2 Then
                SoundPause(A) = 12
            ElseIf A = 3 Then SoundPause(A) = 12
            ElseIf A = 4 Then SoundPause(A) = 12
            ElseIf A = 5 Then SoundPause(A) = 30
            ElseIf A = 8 Then SoundPause(A) = 10
            ElseIf A = 9 Then SoundPause(A) = 4
            ElseIf A = 10 Then SoundPause(A) = 8
            ElseIf A = 12 Then SoundPause(A) = 10
            ElseIf A = 17 Then SoundPause(A) = 10
            ElseIf A = 26 Then SoundPause(A) = 8
            ElseIf A = 31 Then SoundPause(A) = 20
            ElseIf A = 37 Then SoundPause(A) = 10
            ElseIf A = 42 Then SoundPause(A) = 16
            ElseIf A = 50 Then SoundPause(A) = 8
            ElseIf A = 54 Then SoundPause(A) = 8
            ElseIf A = 71 Then SoundPause(A) = 9
            ElseIf A = 74 Then SoundPause(A) = 8
            ElseIf A = 81 Then SoundPause(A) = 5
            ElseIf A = 86 Then SoundPause(A) = 8
            Else
                SoundPause(A) = 4
            End If
        End If
    End If
End Sub

Public Sub BlockSound() 'stops all sound from being played for 10 cycles
    Dim A As Integer
    For A = 1 To numSounds
        SoundPause(A) = 10
    Next A
End Sub

Public Sub UpdateSound() 'checks to loop music and update the soundpause variable
    Dim A As Integer
    Dim sPosition As String * 255
    Dim sLength As String * 255
    Dim position As Single
    Dim Length As Single
    If noSound = True Then Exit Sub
    'If musicPlaying = True Then
    '    musicLoop = musicLoop + 1
    '    If musicName = "music0" Then musicLoop = 0
    '    If musicLoop >= 1 And musicName <> "music0" Then
    '        musicLoop = 0
    '        mciSendString "Status " & musicName & " Position", sPosition, 255, 0
    '        mciSendString "Status " & musicName & " Length", sLength, 255, 0
    '        nLength = InStr(sLength, Chr$(0))
    '        Length = Val(left$(sLength, nLength - 1))
    '        nLength = InStr(sPosition, Chr$(0))
    '        position = Val(left$(sPosition, nLength - 1))
    '        If position >= Length - 70 Then
    '            If PSwitchTime = 0 And PSwitchStop = 0 Then
    '                'mciSendString "stop " & musicName, vbNullString, 0, 0
    '                If GameOutro = False Then mciSendString "play " & musicName & " from 70", vbNullString, 0, 0
    '            End If
    '        End If
    '    End If
    'End If
    For A = 1 To numSounds
        If SoundPause(A) > 0 Then SoundPause(A) = SoundPause(A) - 1
    Next A
End Sub

