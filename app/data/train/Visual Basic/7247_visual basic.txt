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

﻿Public Class FrmModifSuppr

    Private Sub Rbt_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RbtModif.Click, RbtSuppr.Click
        BtnAction.Text = sender.Tag
        Me.Tag = sender.Tag & " "
        Call FrmAccueil.tmrHeureActu_Tick(sender, e)
    End Sub

    Private Sub FrmModifSuppr_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        RbtModif.Tag = "Modifier l'inscription"
        RbtSuppr.Tag = "Supprimer l'inscription"
        Me.Tag = RbtModif.Tag & " "
        Call FrmAccueil.tmrHeureActu_Tick(sender, e)
    End Sub

    Private Sub RbtNum_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RbtNum.Click
        TxtNum.Enabled = True
        CbxNomPre.Enabled = False
    End Sub

    Private Sub RbtNomPre_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RbtNomPre.Click
        TxtNum.Enabled = False
        CbxNomPre.Enabled = True
    End Sub

    Private Sub BtnRet_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnRet.Click
        Me.Hide()
        FrmAccueil.Show()
    End Sub

    Private Sub TxtNum_KeyPress(ByVal sender As Object, ByVal e As KeyPressEventArgs) Handles TxtNum.KeyPress
        If (e.KeyChar = vbBack) Then
            Exit Sub
        End If
        If (e.KeyChar >= "0" And e.KeyChar <= "9") Then
            Exit Sub
        End If
        e.KeyChar = Chr(0)
    End Sub

    Private Sub BtnAction_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnAction.Click
        Dim num As Integer
        If RbtNum.Checked Then
            FileGet(numFicInscrit, tblEnreg, 1)
            If TxtNum.Text = "" Or tblEnreg(TxtNum.Text) <> 1 Then
                MsgBox("Numéro invalide")
                TxtNum.Focus()
                Exit Sub
            End If
            num = TxtNum.Text
        Else 'nom-prénom
            num = LstCacheNum.Items(CbxNomPre.SelectedIndex)
        End If
        FileGet(numFicInscrit, persActu, num)
        If RbtModif.Checked Then
            Me.Hide()
            FrmInscripRens.Show()
            FrmInscripRens.modif = True
            FrmInscripRens.LblNum.Text = num
            FrmInscripRens.Reinit()
        Else 'suppr
            Me.Hide()
            FrmRecapInscription.Show()
            FrmRecapInscription.suppr = True
            FrmRecapInscription.bilan = False
            FrmRecapInscription.modif = False
            FrmRecapInscription.LblNum.Text = num
            FrmRecapInscription.Reinit()
        End If
    End Sub

    Public Sub Reinit()
        RbtModif.Checked = True
        BtnAction.Text = RbtModif.Tag
        RbtNum.Checked = True
        TxtNum.Enabled = True
        TxtNum.Text = ""
        CbxNomPre.Items.Clear()
        LstCacheNum.Items.Clear()
        Call chargerNomPre(CbxNomPre, LstCacheNum)
        CbxNomPre.SelectedIndex = 0
        CbxNomPre.Enabled = False
    End Sub
End Class
