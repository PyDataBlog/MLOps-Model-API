Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass()> Public Class TestCOMGenericFind

    <TestInitialize()> _
    Public Sub SetLogonParameters()
        Common.CommonTestMethods.SetLogonParameters()
    End Sub


    <TestMethod()> _
    Public Sub TestCOMGenericFindOnInergyPorMasterDetailPlus()

        Dim stockCode As String = "A100"
        Dim expectedMass As Integer = 10

        Dim postObject As New Codewell.SysproIntegration.SystemQuery.ComGenericFind.Query
        postObject.TableName = "PorMasterDetail+"
        postObject.ColumnsCollection.Add("ShipmentDate")
        postObject.ColumnsCollection.Add("ArrivalInHarbour")
        postObject.ColumnsCollection.Add("PickupDate")

        Dim where1 As New Codewell.SysproIntegration.SystemQuery.ComGenericFind.Expression
        where1.AndOr = ""
        where1.Column = "PurchaseOrder"
        where1.Condition = Codewell.SysproIntegration.SystemQuery.ComGenericFind.Condition.EQ
        where1.Value = "000000000034341"
        Dim where2 As New Codewell.SysproIntegration.SystemQuery.ComGenericFind.Expression
        where2.AndOr = "And"
        where2.Column = "Line"
        where2.Condition = Codewell.SysproIntegration.SystemQuery.ComGenericFind.Condition.EQ
        where2.Value = "4"

        postObject.WhereCollection.Add(where1)
        postObject.WhereCollection.Add(where2)
        With postObject
            .Post(False)
        End With

        Assert.AreEqual("2012-08-24", Codewell.SysproIntegration.Common.SysproUtilities.CheckXMLOut("", "", postObject.XmlOut, "PorMasterDetail_//Row//PickupDate"))

    End Sub

End Class
