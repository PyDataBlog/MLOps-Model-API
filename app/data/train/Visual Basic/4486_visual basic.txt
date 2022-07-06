Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Navigateur
Imports TesterPile
Imports NSubstitute

<TestClass()> Public Class ControleurTest

    <TestMethod()> Public Sub Back_HistoriqueArriereVide_returneNull()
        'Arrange
        Dim CUT As Controleur = New Controleur()
        Dim attendue As String = String.Empty

        'Act
        Dim obtenue As String = CUT.Back()

        'Assert
        Assert.AreEqual(attendue, obtenue)
    End Sub

    <TestMethod()> Public Sub Forward_HistoriqueAvantVide_returneNull()
        'Arrange
        Dim CUT As Controleur = New Controleur()
        Dim attendue As String = String.Empty

        'Act
        Dim obtenue As String = CUT.Forward()

        'Assert
        Assert.AreEqual(attendue, obtenue)
    End Sub

    <TestMethod()> Public Sub Back_HistoriqueArriereContientUnElement_returneElement()
        'Arrange
        Dim StubPile As IPile = Substitute.For(Of IPile)()
        StubPile.Depiler().Returns("Uno")
        Dim CUT As Controleur = New Controleur(StubPile, StubPile)
        Dim attendue As String = "Uno"

        'Act
        Dim obtenue As String = CUT.Back()

        'Assert
        Assert.AreEqual(attendue, obtenue)
    End Sub

    <TestMethod()> Public Sub Forward_HistoriqueAvantContientUnElement_returneElement()
        'Arrange
        Dim StubPile As IPile = Substitute.For(Of IPile)()
        StubPile.Depiler().Returns("Uno")
        Dim CUT As Controleur = New Controleur(StubPile, StubPile)
        Dim attendue As String = "Uno"

        'Act
        Dim obtenue As String = CUT.Forward()

        'Assert
        Assert.AreEqual(attendue, obtenue)

    End Sub

End Class