Public Class screenSelectVolume

    Private Sub screenSelectVolume_Load(sender As Object, e As EventArgs) Handles Me.Load
        screenHome.Hide()
        picCircle.Visible = False
        picHexagon.Visible = False
        picPentagon.Visible = False
        picRectangle.Visible = False
        picSquare.Visible = False
        picTriangle.Visible = False
    End Sub

    Private Sub CircleClick(sender As Object, e As EventArgs) Handles lblCircle.Click, picCircle.Click
        ShapeNum = 0
        screenVolume.Show()
    End Sub

    Private Sub SquareClick(sender As Object, e As EventArgs) Handles lblSquare.Click, picSquare.Click
        ShapeNum = 1
        screenVolume.Show()
    End Sub

    Private Sub HexagonClick(sender As Object, e As EventArgs) Handles lblHexagon.Click, picHexagon.Click
        ShapeNum = 3
        screenVolume.Show()
    End Sub

    Private Sub PentagonClick(sender As Object, e As EventArgs) Handles lblPentagon.Click, picPentagon.Click
        ShapeNum = 4
        screenVolume.Show()
    End Sub

    Private Sub RectangleClick(sender As Object, e As EventArgs) Handles lblRectangle.Click, picRectangle.Click
        ShapeNum = 5
        screenVolume.Show()
    End Sub

    Private Sub TriangleClick(sender As Object, e As EventArgs) Handles lblTriangle.Click, picTriangle.Click
        ShapeNum = 2
        screenVolume.Show()
    End Sub

    Private Sub lblCircle_Leave(sender As Object, e As EventArgs) Handles lblCircle.Leave, picCircle.MouseLeave
        lblCircle.Parent = Me
        lblCircle.BackColor = Color.Transparent
        picCircle.Visible = False
    End Sub

    Private Sub lblHexagon_Leave(sender As Object, e As EventArgs) Handles lblHexagon.Leave, picHexagon.MouseLeave
        lblHexagon.Parent = Me
        lblHexagon.BackColor = Color.Transparent
        picHexagon.Visible = False
    End Sub

    Private Sub lblPentagon_Leave(sender As Object, e As EventArgs) Handles lblPentagon.Leave, picPentagon.MouseLeave
        lblPentagon.Parent = Me
        lblPentagon.BackColor = Color.Transparent
        picPentagon.Visible = False
    End Sub

    Private Sub lblRectangle_Leave(sender As Object, e As EventArgs) Handles lblRectangle.Leave, picRectangle.MouseLeave
        lblRectangle.Parent = Me
        lblRectangle.BackColor = Color.Transparent
        picRectangle.Visible = False
    End Sub

    Private Sub lblSquare_Leave(sender As Object, e As EventArgs) Handles lblSquare.Leave, picSquare.MouseLeave
        lblSquare.Parent = Me
        lblSquare.BackColor = Color.Transparent
        picSquare.Visible = False
    End Sub

    Private Sub lblTriangle_Leave(sender As Object, e As EventArgs) Handles lblTriangle.Leave, picTriangle.MouseLeave
        lblTriangle.Parent = Me
        lblTriangle.BackColor = Color.Transparent
        picTriangle.Visible = False
    End Sub

    Private Sub lblCircle_MouseEnter(sender As Object, e As EventArgs) Handles lblCircle.MouseEnter
        lblCircle.Parent = picCircle
        lblCircle.BackColor = Color.Transparent
        picCircle.Visible = True
    End Sub

    Private Sub lblHexagon_MouseEnter(sender As Object, e As EventArgs) Handles lblHexagon.MouseEnter
        lblHexagon.Parent = picHexagon
        lblHexagon.BackColor = Color.Transparent
        picHexagon.Visible = True
    End Sub

    Private Sub lblPentagon_MouseEnter(sender As Object, e As EventArgs) Handles lblPentagon.MouseEnter
        lblPentagon.Parent = picPentagon
        lblPentagon.BackColor = Color.Transparent
        picPentagon.Visible = True
    End Sub

    Private Sub lblRectangle_MouseEnter(sender As Object, e As EventArgs) Handles lblRectangle.MouseEnter
        lblRectangle.Parent = picRectangle
        lblRectangle.BackColor = Color.Transparent
        picRectangle.Visible = True
    End Sub

    Private Sub lblSquare_MouseEnter(sender As Object, e As EventArgs) Handles lblSquare.MouseEnter
        lblSquare.Parent = picSquare
        lblSquare.BackColor = Color.Transparent
        picSquare.Visible = True
    End Sub

    Private Sub lblTriangle_MouseEnter(sender As Object, e As EventArgs) Handles lblTriangle.MouseEnter
        lblTriangle.Parent = picTriangle
        lblTriangle.BackColor = Color.Transparent
        picTriangle.Visible = True
    End Sub

    Private Sub PictureBox2_Click(sender As Object, e As EventArgs) Handles PictureBox2.Click
        screenExit.Show()
    End Sub

    Private Sub PictureBox1_MouseEnter(sender As Object, e As EventArgs) Handles PictureBox1.MouseEnter
        PictureBox1.BackColor = ColorTranslator.FromHtml("#FFDE21")
    End Sub

    Private Sub PictureBox1_MouseLeave(sender As Object, e As EventArgs) Handles PictureBox1.MouseLeave
        PictureBox1.BackColor = Nothing
    End Sub

    Private Sub PictureBox2_MouseEnter(sender As Object, e As EventArgs) Handles PictureBox2.MouseEnter
        PictureBox2.BackColor = ColorTranslator.FromHtml("#FFDE21")
    End Sub

    Private Sub PictureBox2_MouseLeave(sender As Object, e As EventArgs) Handles PictureBox2.MouseLeave
        PictureBox2.BackColor = Nothing
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        screenHome.Show()
        Me.Close()
    End Sub
End Class