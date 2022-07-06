Public Class Form1

    Private Sub btnCalc_Click(sender As Object, e As EventArgs) Handles btnCacl.Click
        'Longtitude and Latitude For Rect(Variable)
        Dim Long_1 As Double
        Dim Lat_1 As Double
        Dim Long_2 As Double
        Dim Lat_2 As Double
        Dim Long_3 As Double
        Dim Lat_3 As Double
        Dim Long_4 As Double
        Dim Lat_4 As Double
        'Origin Longtitude and Latitude (Variable)
        Dim Origin_Long As Double
        Dim Origin_Lat As Double
        'Rect
        Dim a1, a2, a3, a4 As Double
        'Origin
        Dim b1, b2, b3, b4 As Double
        'Area
        Dim RectArea As Double
        Dim OriginArea As Double
        Dim u1, u2, u3, u4, Area1, Area2, Area3, Area4 As Double


        'Enter value for Longtitude and latitude for origin
        Origin_Long = InputBox("Enter the Longtitde that you at")
        Origin_Lat = InputBox("Enter the Latitude that you at")

        'Enter value for Longtitude and latitude for rect
        Long_1 = InputBox("Enter Longtitude for Point 1")
        Lat_1 = InputBox("Enter Latitude for Point 1")
        Long_2 = InputBox("Enter Longtitude for Point 2")
        Lat_2 = InputBox("Enter Latitude for Point 2")
        Long_3 = InputBox("Enter Longtitude for Point 3")
        Lat_3 = InputBox("Enter Latitude for Point 3")
        Long_4 = InputBox("Enter Longtitude for Point 4")
        Lat_4 = InputBox("Enter Latitude for Point 4")

        lblLongtitude_1.Text = Long_1
        lblLongtitude_2.Text = Long_2
        lblLongtitude_3.Text = Long_3
        lblLongtitude_4.Text = Long_4
        lblLatitude_1.Text = Lat_1
        lblLatitude_2.Text = Lat_2
        lblLatitude_3.Text = Lat_3
        lblLatitude_4.Text = Lat_4

        lblOrigin_Long.Text = Origin_Long
        lblOrigin_Lat.Text = Origin_Lat


        'Line segments for rect
        a1 = Math.Sqrt((Long_1 - Long_2) ^ 2 + (Lat_1 - Lat_2) ^ 2)
        a2 = Math.Sqrt((Long_2 - Long_3) ^ 2 + (Lat_2 - Lat_3) ^ 2)
        a3 = Math.Sqrt((Long_3 - Long_4) ^ 2 + (Lat_3 - Lat_4) ^ 2)
        a4 = Math.Sqrt((Long_4 - Long_1) ^ 2 + (Lat_4 - Lat_1) ^ 2)

        'Line segments for origin
        b1 = Math.Sqrt((Long_1 - Origin_Long) ^ 2 + (Lat_1 - Origin_Lat) ^ 2)
        b2 = Math.Sqrt((Long_2 - Origin_Long) ^ 2 + (Lat_2 - Origin_Lat) ^ 2)
        b3 = Math.Sqrt((Long_3 - Origin_Long) ^ 2 + (Lat_3 - Origin_Lat) ^ 2)
        b4 = Math.Sqrt((Long_4 - Origin_Long) ^ 2 + (Lat_4 - Origin_Lat) ^ 2)

        'Calculate Area
        u1 = (a1 + b1 + b2) / 2
        u2 = (a2 + b2 + b3) / 2
        u3 = (a3 + b3 + b4) / 2
        u4 = (a4 + b4 + b1) / 2
        'Rect
        Area1 = Math.Sqrt(u1 * (u1 - a1) * (u1 - b1) * (u1 - b2))
        Area2 = Math.Sqrt(u2 * (u2 - a2) * (u2 - b2) * (u2 - b3))
        Area3 = Math.Sqrt(u3 * (u3 - a3) * (u3 - b3) * (u3 - b4))
        Area4 = Math.Sqrt(u4 * (u4 - a4) * (u4 - b4) * (u4 - b1))

        RectArea = a1 * a2
        lblRectArea.Text = RectArea
        OriginArea = Area1 + Area2 + Area3 + Area4
        lblOriginArea.Text = OriginArea

        If lblRectArea.Text = lblOriginArea.Text Then
            MessageBox.Show("True")
        Else
            MessageBox.Show("False")
        End If

    End Sub

