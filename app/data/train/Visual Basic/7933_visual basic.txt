Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Codewell.SysproIntegration.SalesOrdersPrimaryPosting.SalesOrderImport
Imports sorData = Codewell.SysproIntegration.SalesOrdersPrimaryPosting.BuildSalesOrderLine.BuildData

<TestClass()> _
Public Class TestBuildSalesOrderLine


    <TestInitialize()> _
    Public Sub SetLogonParameters()
        Common.CommonTestMethods.SetLogonParameters()
    End Sub

    <TestMethod()> _
    Public Sub TestBuildSOLine()

       
        Dim so As New sorData.Build()
        so.Parameters.IncludePriceList = Codewell.SysproIntegration.SalesOrdersPrimaryPosting.BuildSalesOrderLine.BuildData.IncludePriceList.Y


        Assert.IsTrue(so.XMLData.Contains("IncludePriceList"))

    End Sub


End Class
