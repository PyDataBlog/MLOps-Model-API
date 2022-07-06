Public Class TreeViewForm
    Private Sub TreeViewForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Tools.Show()
        Tools.BringToFront()
    End Sub

    Public Sub PrintNodeTreeView(Node As Node, topNode As TreeNode)
        Dim treeTopNode As New TreeNode
        treeTopNode.Text = node.NodeValue
        treeTopNode.ToolTipText = node.NodeValue
        topNode.Nodes.Add(treeTopNode)

        For Each SubNode As Node In node.ListOutwardNodes
            PrintNodeTreeView(SubNode, treeTopNode)
        Next

    End Sub

End Class
