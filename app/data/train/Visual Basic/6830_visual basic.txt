Imports System.ComponentModel
Imports System.IO
Imports System.Text
Imports Newtonsoft.Json.Linq

Public Class _5_RomsLoadingProcessControl

    ''' <summary>
    ''' Parent con tainer control
    ''' </summary>
    Private _parent As RomsContainerControl

    ''' <summary>
    ''' Main progress Text to display
    ''' </summary>
    Private _mainProgressText As String

    ''' <summary>
    ''' Last line of waiting details to display
    ''' </summary>
    Private _workerDetailsLastLine As String = String.Empty

    ''' <summary>
    ''' Indicates if the background worker's job is done
    ''' </summary>
    Private _isJobDone As Boolean = False

    ''' <summary>
    ''' Component initialization
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Private Sub _5_RomsLoadingProcessControl_Load(sender As Object, e As EventArgs) Handles Me.Load
        Try
            Me._parent = TryCast(Me.Parent.Parent, RomsContainerControl)

            If Me._parent Is Nothing Then Throw New Exception("Unexpected error")

            Me.ActionPanel.Visible = False

            Dim collection As List(Of String) = GetFilesAssociatedToExtensions(Me._parent.RomsPath, Me._parent.RomsExtensions)

            If collection.Count > 0 Then
                With Me.ActionWaitingControl
                    .Visible = True
                    .HeaderLabel.Text = "Please wait while data and media's URL are retrieved from screenscraper.fr"
                    .DetailsTextBox.Visible = True
                    .DetailsProgressBar.Visible = True
                    .MainProgressLabel.Visible = True

                    .MainWaitingProgressBar.Style = ProgressBarStyle.Continuous
                    .MainWaitingProgressBar.Maximum = collection.Count
                End With

                Me._parent.RomsData.Clear()
                Me._parent.RomsDataErrors.Clear()

                Me._parent.ButtonNext.Enabled = False
                Me._parent.ButtonPrevious.Enabled = False

                ' start asynchrone screenscraper system list loading
                LoadBackgroundWorker.RunWorkerAsync()
            Else
                Throw New Exception("Oups ! It seems that your roms have disapeared from the selected folder.")
            End If

        Catch ex As Exception
            ShowErrorMessage(ex)
        End Try
    End Sub

    ''' <summary>
    ''' Retrieve the games data
    ''' </summary>
    ''' <param name="worker"></param>
    ''' <param name="e"></param>
    ''' <returns></returns>
    Private Function GetGamesData(ByVal worker As BackgroundWorker, ByVal e As DoWorkEventArgs) As Boolean
        Dim result As Boolean = False
        Dim cpt As Integer = 0

        Dim webClient As New System.Net.WebClient()
        Dim query As String = String.Empty
        Dim json As String

        Try
            Dim collection As List(Of String) = GetFilesAssociatedToExtensions(Me._parent.RomsPath, Me._parent.RomsExtensions)

            If collection.Count > 0 Then

                For Each file As String In collection
                    Dim rom As RomsDataSet.SSRomsRow = Me._parent.RomsData.NewSSRomsRow
                    Dim fileName As String

                    fileName = file.Substring(0, file.Length - 4)
                    fileName = fileName.Substring(fileName.LastIndexOf("\") + 1)

                    rom.extension = file.Substring(file.LastIndexOf("."))
                    rom.filename = fileName

                    Me._mainProgressText = rom.filename

                    rom.size = New FileInfo(file).Length
                    If rom.size <= 15000000 Then
                        rom.local_rommd5 = md5_hash(file)
                    End If

                    ' interrogation
                    query = BuildGenericURL("jeuInfos.php")
                    query = query & "&crc="
                    query = query & "&md5=" & rom.local_rommd5
                    query = query & "&sha1="
                    query = query & "&systemid=" & Me._parent.ScreenScraperSelectedSystem
                    query = query & "&romnom=" & rom.filename
                    query = query & "&romtaille=" & rom.size

                    ' getting result
                    webClient.Encoding = Encoding.UTF8
                    json = webClient.DownloadString(query)

                    If Not String.IsNullOrEmpty(json) _
                        And Not json.Contains("Erreur : Rom/Iso/Dossier non trouvée !") _
                        And Not json.Contains("Erreur : Jeu non trouvée !") Then

                        GetGameData(json, rom)
                    Else
                        'second test without md5 hash (only try to get game with its name)

                        ' interrogation
                        query = BuildGenericURL("jeuInfos.php")
                        query = query & "&crc="
                        query = query & "&md5="
                        query = query & "&sha1="
                        query = query & "&systemid=" & Me._parent.ScreenScraperSelectedSystem
                        query = query & "&romnom=" & rom.filename
                        query = query & "&romtaille="

                        ' getting result
                        webClient.Encoding = Encoding.UTF8
                        json = webClient.DownloadString(query)

                        If Not String.IsNullOrEmpty(json) _
                            And Not json.Contains("Erreur : Rom/Iso/Dossier non trouvée !") _
                            And Not json.Contains("Erreur : Jeu non trouvée !") Then

                            GetGameData(json, rom)
                        Else
                            Me._workerDetailsLastLine = "<<< - !!! ERROR !!! " & rom.filename & " not found on screenscraper.fr !!! - >>>" & vbCrLf

                            Dim err As RomsDataSet.ErrorsSSRomsLoadingRow
                            err = Me._parent.RomsDataErrors.NewErrorsSSRomsLoadingRow()
                            err.type = LogsType.Loading.ToString
                            err.filename = rom.filename
                            err.details = json
                            Me._parent.RomsDataErrors.AddErrorsSSRomsLoadingRow(err)

                            AddLogs(New Exception("Error retrieving data from screenscraper.fr for : " & rom.filename & " with return : " & json))

                            ' TODO make an option to add to romlist games not found on the romlist
                            ' add games to list even they are in error to get them in the romlist
                            If Me._parent.IsIncludeNotFoundGames Then
                                rom.nom_us = rom.filename
                                rom.nom_jp = rom.filename
                                rom.nom_wor = rom.filename
                                rom.nom_eu = rom.filename

                                Me._parent.RomsData.AddSSRomsRow(rom)
                            End If

                        End If
                    End If

                    cpt = cpt + 1
                    worker.ReportProgress(cpt)

                Next

                result = True
            Else
                Throw New Exception("Oups ! It seems that your roms have disapeared from the selected folder.")
            End If

        Catch ex As Exception
            Throw ex
        End Try

        Return result
    End Function

    ''' <summary>
    ''' Retrieve the data for a specific game
    ''' </summary>
    ''' <param name="json"></param>
    ''' <param name="rom"></param>
    Private Sub GetGameData(ByRef json As String, ByRef rom As RomsDataSet.SSRomsRow)
        Try
            Dim o As JObject = JObject.Parse(json)

            For Each s As JToken In o.SelectToken("response.jeu")
                'nom
                If Me._parent.IsUseFileNameForGameTitle = True Then
                    rom.nom_us = rom.filename
                    rom.nom_jp = rom.filename
                    rom.nom_wor = rom.filename
                    rom.nom_eu = rom.filename
                Else
                    GetData(o, rom.nom_us, "response.jeu.noms.nom_us")
                    GetData(o, rom.nom_jp, "response.jeu.noms.nom_jp")
                    GetData(o, rom.nom_wor, "response.jeu.noms.nom_wor")
                    GetData(o, rom.nom_eu, "response.jeu.noms.nom_eu")

                    If rom.nom_us = String.Empty _
                       And rom.nom_jp = String.Empty _
                       And rom.nom_wor = String.Empty _
                       And rom.nom_eu = String.Empty Then

                        GetData(o, rom.nom_us, "response.jeu.nom")
                        GetData(o, rom.nom_jp, "response.jeu.nom")
                        GetData(o, rom.nom_wor, "response.jeu.nom")
                        GetData(o, rom.nom_eu, "response.jeu.nom")

                        If rom.nom_us = String.Empty _
                           And rom.nom_jp = String.Empty _
                           And rom.nom_wor = String.Empty _
                           And rom.nom_eu = String.Empty Then

                            rom.nom_jp = rom.filename
                            rom.nom_wor = rom.filename
                            rom.nom_eu = rom.filename
                        End If
                    End If
                End If

                'cloneof
                GetData(o, rom.cloneof, "response.jeu.cloneof")
                'date
                GetData(o, rom.date_us, "response.jeu.dates.date_us")
                GetData(o, rom.date_jp, "response.jeu.dates.date_jp")
                GetData(o, rom.date_wor, "response.jeu.dates.date_wor")
                GetData(o, rom.date_eu, "response.jeu.dates.date_eu")
                ' keep only year
                If Not String.IsNullOrEmpty(rom.date_us) And rom.date_us.Length >= 4 Then rom.date_us = rom.date_us.Substring(0, 4)
                If Not String.IsNullOrEmpty(rom.date_jp) And rom.date_jp.Length >= 4 Then rom.date_jp = rom.date_jp.Substring(0, 4)
                If Not String.IsNullOrEmpty(rom.date_wor) And rom.date_wor.Length >= 4 Then rom.date_wor = rom.date_wor.Substring(0, 4)
                If Not String.IsNullOrEmpty(rom.date_eu) And rom.date_eu.Length >= 4 Then rom.date_eu = rom.date_eu.Substring(0, 4)

                'editeur
                GetData(o, rom.editeur, "response.jeu.editeur")
                'genre
                GetData(o, rom.genres_en, "response.jeu.genres.genres_en")
                GetData(o, rom.genres_fr, "response.jeu.genres.genres_fr")
                GetData(o, rom.genres_de, "response.jeu.genres.genres_de")
                GetData(o, rom.genres_es, "response.jeu.genres.genres_es")
                GetData(o, rom.genres_pt, "response.jeu.genres.genres_pt")
                'joueurs
                GetData(o, rom.joueurs, "response.jeu.joueurs")
                'rotation
                GetData(o, rom.rotation, "response.jeu.rotation")
                'controles
                GetData(o, rom.controles, "response.jeu.controles")
                'media_screenshot
                GetData(o, rom.media_screenshot, "response.jeu.medias.media_screenshot")
                'media_screenshottitle
                GetData(o, rom.media_screenshottitle, "response.jeu.medias.media_screenshottitle")
                'media_fanart
                GetData(o, rom.media_fanart, "response.jeu.medias.media_fanart")
                'media_video
                GetData(o, rom.media_video, "response.jeu.medias.media_video")
                'media_wheel
                GetData(o, rom.media_wheel_us, "response.jeu.medias.media_wheels.media_wheel_us")
                GetData(o, rom.media_wheel_jp, "response.jeu.medias.media_wheels.media_wheel_jp")
                GetData(o, rom.media_wheel_wor, "response.jeu.medias.media_wheels.media_wheel_wor")
                'media_wheelcarbon
                GetData(o, rom.media_wheelcarbon_us, "response.jeu.medias.media_wheelscarbon.media_wheelcarbon_us")
                GetData(o, rom.media_wheelcarbon_jp, "response.jeu.medias.media_wheelscarbon.media_wheelcarbon_jp")
                GetData(o, rom.media_wheelcarbon_wor, "response.jeu.medias.media_wheelscarbon.media_wheelcarbon_wor")
                'media_wheelsteel
                GetData(o, rom.media_wheelsteel_us, "response.jeu.medias.media_wheelssteel.media_wheelsteel_us")
                GetData(o, rom.media_wheelsteel_jp, "response.jeu.medias.media_wheelssteel.media_wheelsteel_jp")
                GetData(o, rom.media_wheelsteel_wor, "response.jeu.medias.media_wheelssteel.media_wheelsteel_wor")
                'media_boxtexture
                GetData(o, rom.media_boxtexture_us, "response.jeu.medias.media_boxs.media_boxstexture.media_boxtexture_us")
                GetData(o, rom.media_boxtexture_jp, "response.jeu.medias.media_boxs.media_boxstexture.media_boxtexture_jp")
                GetData(o, rom.media_boxtexture_eu, "response.jeu.medias.media_boxs.media_boxstexture.media_boxtexture_eu")
                'media_box2d
                GetData(o, rom.media_box2d_us, "response.jeu.medias.media_boxs.media_boxs2d.media_box2d_us")
                GetData(o, rom.media_box2d_jp, "response.jeu.medias.media_boxs.media_boxs2d.media_box2d_jp")
                GetData(o, rom.media_box2d_eu, "response.jeu.medias.media_boxs.media_boxs2d.media_box2d_eu")
                'media_box2d-side
                GetData(o, rom.media_box2d_side_us, "response.jeu.medias.media_boxs.media_boxs2d-side.media_box2d-side_us")
                GetData(o, rom.media_box2d_side_jp, "response.jeu.medias.media_boxs.media_boxs2d-side.media_box2d-side_jp")
                GetData(o, rom.media_box2d_side_eu, "response.jeu.medias.media_boxs.media_boxs2d-side.media_box2d-side_eu")
                'media_box2d-back
                GetData(o, rom.media_box2d_back_us, "response.jeu.medias.media_boxs.media_boxs2d-back.media_box2d-back_us")
                GetData(o, rom.media_box2d_back_jp, "response.jeu.medias.media_boxs.media_boxs2d-back.media_box2d-back_jp")
                GetData(o, rom.media_box2d_back_eu, "response.jeu.medias.media_boxs.media_boxs2d-back.media_box2d-back_eu")
                'media_box3d
                GetData(o, rom.media_box3d_us, "response.jeu.medias.media_boxs.media_boxs3d.media_box3d_us")
                GetData(o, rom.media_box3d_jp, "response.jeu.medias.media_boxs.media_boxs3d.media_box3d_jp")
                GetData(o, rom.media_box3d_eu, "response.jeu.medias.media_boxs.media_boxs3d.media_box3d_eu")
                'media_supporttexture_us
                GetData(o, rom.media_box3d_eu, "response.jeu.medias.media_supports.media_supportstexture.media_supporttexture_us")
                GetData(o, rom.media_box3d_jp, "response.jeu.medias.media_supports.media_supportstexture.media_supporttexture_jp")
                GetData(o, rom.media_box3d_eu, "response.jeu.medias.media_supports.media_supportstexture.media_supporttexture_eu")
                'media_support2d
                GetData(o, rom.media_support2d_eu, "response.jeu.medias.media_supports.media_supports2d.media_support2d_us")
                GetData(o, rom.media_support2d_jp, "response.jeu.medias.media_supports.media_supports2d.media_support2d_jp")
                GetData(o, rom.media_support2d_eu, "response.jeu.medias.media_supports.media_supports2d.media_support2d_eu")
            Next
        Catch ex As Exception
            Me._workerDetailsLastLine = "<<< - !!! ERROR !!! " & rom.filename & " not found on screenscraper.fr !!! - >>>" & vbCrLf

            Dim err As RomsDataSet.ErrorsSSRomsLoadingRow
            err = Me._parent.RomsDataErrors.NewErrorsSSRomsLoadingRow()
            err.type = LogsType.Loading.ToString
            err.filename = rom.filename
            err.details = ex.Message
            Me._parent.RomsDataErrors.AddErrorsSSRomsLoadingRow(err)

            AddLogs(New Exception("Error retrieving data from screenscraper.fr for : " & rom.filename & " with return : " & ex.Message))
        End Try

        Me._workerDetailsLastLine = "Building index for " & rom.filename & " Ok" & vbCrLf

#Region "list of ss roms columns"
        'filename
        'extension
        'size
        'nom_us
        'nom_jp
        'nom_wor
        'nom_eu
        'cloneof
        'date_us
        'date_jp
        'date_wor
        'date_eu
        'editeur
        'genres_en
        'genres_fr
        'genres_de
        'genres_es
        'genres_pt
        'genres_pt
        'joueurs
        'rotation
        'controles
        'media_screenshot
        'media_screenshottitle
        'media_fanart
        'media_video
        'media_wheel_us
        'media_wheel_jp
        'media_wheel_wor
        'media_wheelcarbon_us
        'media_wheelcarbon_jp
        'media_wheelcarbon_wor
        'media_wheelsteel_us
        'media_wheelsteel_jp
        'media_wheelsteel_wor
        'media_boxtexture_us
        'media_boxtexture_jp
        'media_boxtexture_eu
        'media_box2d_us
        'media_box2d_jp
        'media_box2d_eu
        'media_box2d_side_us
        'media_box2d_side_jp
        'media_box2d_side_eu
        'media_box2d_back_us
        'media_box2d_back_jp
        'media_box2d_back_eu
        'media_box3d_us
        'media_box3d_jp
        'media_box3d_eu
        'media_supporttexture_us
        'media_supporttexture_jp
        'media_supporttexture_eu
        'media_support2d_us
        'media_support2d_jp
        'media_support2d_eu
        'local_romsize
        'local_romcrc
        'local_rommd5
#End Region
        rom.isScraped = False
        Me._parent.RomsData.AddSSRomsRow(rom)
    End Sub

    ''' <summary>
    ''' Get the given JSon node 
    ''' </summary>
    ''' <param name="o"></param>
    ''' <param name="value"></param>
    ''' <param name="path"></param>
    Private Sub GetData(ByRef o As JObject, ByRef value As String, ByRef path As String)
        Try
            If o.SelectToken(path, False) IsNot Nothing Then
                Dim t As Type = o.SelectToken(path, False).GetType

                Select Case t
                    Case GetType(JValue)
                        value = o.SelectToken(path, False)
                    Case GetType(JArray)
                        Dim b As New StringBuilder
                        Dim cpt As Integer = 0


                        For Each e In o.SelectToken(path, False)
                            b.Append("/" & e.ToString)

                            cpt = cpt + 1
                        Next
                        b.Remove(0, 1)
                        value = b.ToString.Trim
                    Case Else
                End Select
            Else
                value = String.Empty
            End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub

    ''' <summary>
    ''' Defines the asynchronous Job for loading Screen Scraper data
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Private Sub LoadBackgroundWorker_DoWork(sender As Object, e As DoWorkEventArgs) Handles LoadBackgroundWorker.DoWork
        Dim worker As BackgroundWorker = CType(sender, BackgroundWorker)
        e.Result = GetGamesData(worker, e)
    End Sub

    ''' <summary>
    ''' Update waiting progress update
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Private Sub LoadBackgroundWorker_ProgressChanged(sender As Object, e As ProgressChangedEventArgs) Handles LoadBackgroundWorker.ProgressChanged
        With Me.ActionWaitingControl
            .MainProgressLabel.Text = Me._mainProgressText
            .MainWaitingProgressBar.Value = e.ProgressPercentage

            .DetailsTextBox.AppendText(Me._workerDetailsLastLine)
        End With
    End Sub

    ''' <summary>
    ''' Is triggered when Screen Scraper Games data loading is finished
    ''' </summary>
    ''' <param name="sender"></param>
    ''' <param name="e"></param>
    Private Sub LoadBackgroundWorker_RunWorkerCompleted(sender As Object, e As RunWorkerCompletedEventArgs) Handles LoadBackgroundWorker.RunWorkerCompleted
        If (e.Error Is Nothing) Or Not e.Cancelled Then
            Me.ActionWaitingControl.DetailsProgressBar.Visible = False
            Me.ActionWaitingControl.MainProgressLabel.Visible = False
            Me.ActionWaitingControl.HeaderLabel.Text = "Finished !"

            If Me._parent.RomsDataErrors.Count > 0 Then
                Dim sb As New StringBuilder
                sb.Append(vbCrLf & vbCrLf & "Some errors occured during process : " & vbCrLf)

                For Each err As RomsDataSet.ErrorsSSRomsLoadingRow In Me._parent.RomsDataErrors.Select("", "filename ASC")
                    sb.Append("    - " & err.filename & " : " & err.details & vbCrLf)
                Next

                Me.ActionWaitingControl.DetailsTextBox.Text = ""
                Me.ActionWaitingControl.DetailsTextBox.AppendText(sb.ToString)
                Me.ActionWaitingControl.DetailsTextBox.Enabled = True
            Else
                Me.ActionWaitingControl.DetailsTextBox.Visible = False
            End If

            Me._parent.ButtonPrevious.Enabled = True
            Me._parent.ButtonNext.Enabled = True
        End If
    End Sub

End Class
