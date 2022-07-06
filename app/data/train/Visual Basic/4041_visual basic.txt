Module Musik
    Public Sub Launcher_Musik_Play()

        If Datenbank.Launcher_Musik_Modus = True Then
            My.Computer.Audio.Play(Datenbank.Spiel_Pfad & Datenbank.Spiel_Pfad_Spiel & "/sounds/" & Datenbank.Launcher_Titel_Song & ".wav", AudioPlayMode.BackgroundLoop)
        Else
            My.Computer.Audio.Stop()
        End If

    End Sub

    Public Sub Spielmenu_Titel_Play()

        If Datenbank.Launcher_Musik_Modus = True Then
            My.Computer.Audio.Play(Datenbank.Spiel_Pfad & Datenbank.Spiel_Pfad_Spiel & "/sounds/" & Datenbank.Launcher_Titel_Song & ".wav", AudioPlayMode.BackgroundLoop)
        Else
            My.Computer.Audio.Stop()
        End If

    End Sub
End Module
