Imports System
Imports System.Text
Imports System.Collections.Generic
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports Codewell.SysproIntegration.ARPrimaryPosting

<TestClass()> Public Class TestInvoicePosting

    Private testContextInstance As TestContext

    '''<summary>
    '''Gets or sets the test context which provides
    '''information about and functionality for the current test run.
    '''</summary>
    Public Property TestContext() As TestContext
        Get
            Return testContextInstance
        End Get
        Set(ByVal value As TestContext)
            testContextInstance = Value
        End Set
    End Property

#Region "Additional test attributes"
    '
    ' You can use the following additional attributes as you write your tests:
    '
    ' Use ClassInitialize to run code before running the first test in the class
    <ClassInitialize()> Public Shared Sub MyClassInitialize(ByVal testContext As TestContext)
        Common.CommonTestMethods.SetLogonParameters()
    End Sub
    '
    ' Use ClassCleanup to run code after all tests in a class have run
    ' <ClassCleanup()> Public Shared Sub MyClassCleanup()
    ' End Sub
    '
    ' Use TestInitialize to run code before running each test
    ' <TestInitialize()> Public Sub MyTestInitialize()
    ' End Sub
    '
    ' Use TestCleanup to run code after each test has run
    ' <TestCleanup()> Public Sub MyTestCleanup()
    ' End Sub
    '
#End Region


    <TestMethod()> _
    Public Sub InvoicePostingAllowsMoreThanOneItem()
        Dim postObject As New AccountsReceivableInvoicePosting.Data.PostArInvoices

        Dim item1 As New AccountsReceivableInvoicePosting.Data.Item
        With item1
            .Invoice = "123"
        End With

        Dim item2 As New AccountsReceivableInvoicePosting.Data.Item
        With item2
            .Invoice = "124"
        End With

        postObject.ItemCollection.Add(item1)
        postObject.ItemCollection.Add(item2)

        Assert.IsFalse(postObject.XMLData.Contains("<ItemCollection>"))
        Assert.IsTrue(postObject.XMLData.Contains(">123</Invoice>"))
        Assert.IsTrue(postObject.XMLData.Contains(">124</Invoice>"))

    End Sub

End Class
