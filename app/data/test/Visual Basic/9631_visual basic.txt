'Klasse die reine Spiellogik enthält.
Public Class Game
    Dim actPlayer As Integer = 1
    Public fields(6, 6) As Integer
    Public Sub init()
        isGame = True
        paintField()
        actPlayer = 1
        isWin = False
        lblActPlayer.Text = CStr(actPlayer)
        lblWinner.Text = ""
        For iColumn As Integer = 0 To 6
            For iRow As Integer = 0 To 6
                fields(iColumn, iRow) = 0
            Next
        Next

    End Sub
    'Zeichnet das Spielfeld
    Public Sub paintField()
        Dim graph As Graphics
        Dim Pen As Pen = New Pen(Color.DarkBlue)
        Dim PenWhite As Pen = New Pen(Color.White)
        Dim BrushWhite As Brush = New SolidBrush(Color.White)
        Dim ColumnsWidth As Integer = calcCellWidth()

        graph = gamePanel.CreateGraphics
        graph.Clear(Color.DarkBlue)

        Dim tempMargin As Integer = 0
        For i As Integer = 0 To 6
            graph.DrawLine(PenWhite, tempMargin + ColumnsWidth, 0, tempMargin + ColumnsWidth, gamePanel.Height)
            graph.DrawLine(PenWhite, 0, tempMargin + ColumnsWidth, gamePanel.Width, tempMargin + ColumnsWidth)


            tempMargin = tempMargin + ColumnsWidth
        Next



    End Sub
    'Spielzug
    Public Sub move(ByVal X As Integer)

        Dim Spalte As Integer = calcColumn(X)

        Dim place As Integer = checkStone(Spalte)
        setStone(place, Spalte)
    End Sub
    '
    Public Sub moveWithColumn(ByVal column As Integer)
        Dim place As Integer = checkStone(column)
        setStone(place, column)
    End Sub
    'Berechnet die Breite einer Zelle
    Private Function calcCellWidth() As Integer
        Dim ColumnWidth As Integer = gamePanel.Width \ 6
        Return ColumnWidth

    End Function
    'Berechnet in welcher Spalte der Spieler geklicket hat
    Private Function calcColumn(ByVal X As Integer) As Integer
        Dim cell As Integer = calcCellWidth()
        Dim aktuelleSpalte As Integer = 0
        Dim gefundeneSpalte As Integer = 1
        Dim isFound As Boolean = False
        Do While isFound = False
            If X > aktuelleSpalte And X < aktuelleSpalte + cell Then
                isFound = True
            Else
                aktuelleSpalte = aktuelleSpalte + cell
                gefundeneSpalte += 1
            End If

        Loop
        Return gefundeneSpalte
    End Function
    'Wechselt den Spieler
    Private Sub changePlayer()
        If actPlayer = 1 Then
            actPlayer = 2
        Else
            actPlayer = 1
        End If
        lblActPlayer.Text = CStr(actPlayer)
    End Sub
    'Testet wo der Stein gezeichnet werden muss
    Private Function checkStone(ByVal column As Integer) As Integer
        Dim isEmpty As Boolean = False
        Dim i As Integer = 6
        While isEmpty = False
            If fields(column, i) = 0 Then
                isEmpty = True
            Else
                i -= 1
            End If
        End While
        Return i
    End Function
    'Setzt einen Stein
    Private Sub setStone(ByVal stonePlace As Integer, ByVal spalte As Integer)
        Dim StoneBrush As SolidBrush
        Dim graph As Graphics
        If actPlayer = 1 Then
            StoneBrush = New SolidBrush(Color.Yellow)
        Else
            StoneBrush = New SolidBrush(Color.Green)
        End If

        graph = gamePanel.CreateGraphics
        Dim loc As Point
        loc.X = spalte * calcCellWidth() - calcCellWidth()
        loc.Y = stonePlace * calcCellWidth() - calcCellWidth()
        Dim s As Size
        s.Width = calcCellWidth()
        s.Height = calcCellWidth()
        Dim Rect As Rectangle = New Rectangle(loc, s)
        graph.FillEllipse(StoneBrush, Rect)


        fields(spalte, stonePlace) = actPlayer
        checkWin(spalte, stonePlace)
        changePlayer()
    End Sub
    Private Sub checkWin(ByVal column As Integer, ByVal row As Integer)
        Dim zaehler As Integer = -1

        

        'Zuerst Testen ob Horizontal ein Win besteht
        Dim horizontal As Integer = checkHorizontal(column, row)
        Dim vertical As Integer = checkVertical(column, row)
        If horizontal >= 4 Or vertical >= 4 Then
            isWin = True
            lblWinner.Text = CStr("Spieler " & actPlayer & " hat gewonnen!")
        End If
        

    End Sub
    'Testet ob Horizontal 4 oder mehr Steine sind.
    Private Function checkHorizontal(ByVal column As Integer, ByVal row As Integer) As Integer
        Dim zaehler = 1
        Dim iLeft As Integer = column - 1
        Dim iRight As Integer = column + 1
        Dim iLeftOtherStone As Boolean = False
        Dim iRightOtherStone As Boolean = False

        While iLeft > 0 And iLeftOtherStone = False
            If fields(iLeft, row) = actPlayer Then
                zaehler += 1
            Else
                iLeftOtherStone = True
            End If

            iLeft -= 1
        End While
        While iRight <= 6 And iRightOtherStone = False
            If fields(iRight, row) = actPlayer Then
                zaehler += 1
            Else
                iRightOtherStone = True
            End If
            iRight += 1
        End While
        Return zaehler
    End Function
    'Testet ob Vertikal 4 oder mehr Steine sind.
    Private Function checkVertical(ByVal column As Integer, ByVal row As Integer) As Integer
        Dim zaehler As Integer = 1
        Dim iUp As Integer = row + 1
        Dim iDown As Integer = row - 1
        Dim iUpOtherStone As Boolean = False
        Dim iDownOtherStone As Boolean = False
        While iUp <= 6 And iUpOtherStone = False
            If fields(column, iUp) = actPlayer Then
                zaehler += 1
            Else
                iUpOtherStone = True
            End If
            iUp += 1
        End While
        While iDown > 0 And iDownOtherStone = False
            If fields(column, iDown) = actPlayer Then
                zaehler += 1
            Else
                iDownOtherStone = True
            End If
            iDown -= 1
        End While
        Return zaehler
    End Function
    'Testet ob Diagonal 4 oder mehr Steine sind.
    Private Function checkDiagonal(ByRef column As Integer, ByVal row As Integer) As Integer
        Dim zaehler As Integer = 1
        Dim iUpColumn As Integer = column - 1
        Dim iUpRow As Integer = row - 1

        Dim iDownColumn As Integer = column + 1
        Dim iDownRow As Integer = row + 1
        Dim diaUpOtherStone As Boolean = False
        Dim diaDownOtherStone As Boolean = False

        While iDownColumn <= 6 And iDownRow <= 6 And diaDownOtherStone = False
            If fields(iDownColumn, iDownRow) = actPlayer Then
                zaehler += 1
            Else
                diaDownOtherStone = True
            End If
            iDownColumn += 1
            iDownRow += 1

        End While
        While iUpColumn > 0 And iUpRow > 0 And diaUpOtherStone = False
            If fields(iUpColumn, iUpRow) = actPlayer Then
                zaehler += 1
            Else
                diaUpOtherStone = True
            End If
            iUpColumn -= 1
            iUpRow -= 1
        End While

        Return zaehler
    End Function
    'Auslesen ob ein Spieler gewonnen hat um Events abzufangen
    Public Function isWinner() As Boolean
        Return isWin
    End Function
End Class
