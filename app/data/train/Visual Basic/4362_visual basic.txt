'' Copyright (c) 2013, Kcchouette and b-dauphin on Github
'' All rights reserved.
'' 
'' Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
'' 
''     Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
''     Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
''     Neither the name of the <ORGANIZATION> nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
'' 
'' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

﻿Public Class FrmAccueil

    Private Sub btnInscrire_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnInscrire.Click
        Me.Hide()
        persActu = bidon
        FrmInscripRens.modif = False
        FrmInscripRens.Show()
        FrmInscripRens.Reinit()
    End Sub

    Private Sub FrmAccueil_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated
        tmrHeureActu.Start()
        Call tmrHeureActu_Tick(sender, e)
    End Sub

    Private Sub FrmAccueil_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        tmrHeureActu.Start()
        Me.Tag = Me.Text & " "
        ReDim persActu.tIndEcrit(3)
        ReDim persActu.tIndOral(2)
        ReDim bidon.tIndEcrit(3)
        ReDim bidon.tIndOral(2)
        FileOpen(numFicInscrit, "inscriptions.dat", OpenMode.Random, OpenAccess.ReadWrite, OpenShare.Shared, lgEnr)
        'Call creerFichier()
    End Sub

    Sub tmrHeureActu_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tmrHeureActu.Tick
        On Error Resume Next
        ActiveForm.Text = ActiveForm.Tag & TimeOfDay
    End Sub

    Private Sub btnFin_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnFin.Click
        Me.Hide()
        FrmAffichBilan.fin = True
        FrmAffichBilan.Show()
    End Sub

    Private Sub btnBilan_Click(sender As Object, e As EventArgs) Handles btnBilan.Click
        Me.Hide()
        FrmAffichBilan.Show()
    End Sub

    Private Sub btnAff_Click(sender As Object, e As EventArgs) Handles btnAff.Click
        Me.Hide()
        FrmEtatActuInscrip.Show()
        FrmEtatActuInscrip.Reinit()
    End Sub

    Private Sub btnModifSuppr_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnModifSuppr.Click
        Me.Hide()
        FrmModifSuppr.Show()
        FrmModifSuppr.Reinit()
    End Sub
End Class
