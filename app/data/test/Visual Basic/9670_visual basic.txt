Option Strict Off
Option Explicit On
Friend Class Form1
	Inherits System.Windows.Forms.Form
	Dim NdeDeleted As Short
	Dim Flag As Boolean
	Private Sub Command1_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command1.Click
		Dim i As Object
		NdeDeleted = 0
		Text1.Text = CStr(0)
		Command2.Enabled = True
		Command2.Focus()
		Dim X_Axis, Y_Axis As Object
		'UPGRADE_ISSUE: PictureBox property Picture1.AutoRedraw was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
		Picture1.AutoRedraw = True
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 0) = i 'Node ID
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 1) = GetRnd(Val(Text3.Text)) 'X Coordinate
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 2) = GetRnd(Val(Text4.Text)) 'Y coordinate
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 3) = GetRnd(Val(Text5.Text)) 'Power
			'    If Node(i, 3) < 1000 Then
			'        Node(i, 3) = 1000
			'    End If
			
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 4) = 0 'Tree ID
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Node(i, 5) = 0 ' 1-> Left Node alone ,2-> Right Node alone ,3-> Both Nodes
			'Node(i, 9) = 0 ' 1-> Visited 0-> not visited
			'Node(i, 10) = 0 ' 1-> Visited 0-> not visited
			
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			'UPGRADE_ISSUE: PictureBox method Picture1.Circle was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
			Picture1.Circle (Node(i, 1), Node(i, 2)), 50
		Next i
	End Sub
	
	Private Sub Command10_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command10.Click
		'Picture1.Cls
		ResetStructure()
		Command1.Focus()
	End Sub
	
	Private Sub Command11_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command11.Click
		Dim i As Object
		Command9.Focus()
		Dim Cnt As Short
		Cnt = 0
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Node(i, 10) <> 1 Then
				Cnt = Cnt + 1
			End If
		Next i
		'MsgBox " Is number of nodes to be enumerated is :" & Cnt
		Form2.Text1.Text = Form2.Text1.Text & Cnt & vbTab
	End Sub
	
	Private Sub Command12_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command12.Click
		Dim i As Object
		For i = 1 To 100
			Command1_Click(Command1, New System.EventArgs())
			Command2_Click(Command2, New System.EventArgs())
			Command3_Click(Command3, New System.EventArgs())
			Command6_Click(Command6, New System.EventArgs())
			Command11_Click(Command11, New System.EventArgs())
			Command9_Click(Command9, New System.EventArgs())
			Command10_Click(Command10, New System.EventArgs())
		Next i
	End Sub
	
	Private Sub Command2_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command2.Click
		Dim j As Object
		Dim i As Object
		Command3.Enabled = True
		Command3.Focus()
		Dim Y2, Y1, Distance, X1, X2, k As Object
		Dim q, p, Temp As Object
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object k. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			k = 10
			For j = 1 To MaxNode
				'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				If i <> j Then
					'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object X1. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					X1 = Node(i, 1) - Node(j, 1)
					'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object X2. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					X2 = Node(i, 1) - Node(j, 1)
					'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object Y1. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					Y1 = Node(i, 2) - Node(j, 2)
					'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object Y2. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					Y2 = Node(i, 2) - Node(j, 2)
					'UPGRADE_WARNING: Couldn't resolve default property of object Y2. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object Y1. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object X2. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object X1. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object Distance. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					Distance = System.Math.Sqrt((X1 * X2) + (Y1 * Y2))
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object Distance. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					If Distance <= Node(i, 3) Then
						'UPGRADE_WARNING: Couldn't resolve default property of object k. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						k = k + 1
						'UPGRADE_WARNING: Couldn't resolve default property of object k. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object j. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						Node(i, k) = j
						'Picture1.Line (Node(i, 1), Node(i, 2))-(Node(j, 1), Node(j, 2))
					End If
				End If
			Next j
			'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			p = 11
			'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			While (Node(i, p) <> 0)
				'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				q = p + 1
				'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				While (Node(i, q) <> 0)
					'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					If Node(i, p) > Node(i, q) Then
						'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						Temp = Node(i, p)
						'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						Node(i, p) = Node(i, q)
						'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
						Node(i, q) = Temp
					End If
					
					'MsgBox Node(i, p)
					'UPGRADE_WARNING: Couldn't resolve default property of object q. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
					q = q + 1
				End While
				'UPGRADE_WARNING: Couldn't resolve default property of object p. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				p = p + 1
			End While
			
		Next i
	End Sub
	
	Private Sub Command3_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command3.Click
		Dim i As Object
		Command6.Enabled = True
		Command6.Focus()
		Dim PollNode As Object
		'UPGRADE_WARNING: Couldn't resolve default property of object PollNode. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		PollNode = GetRnd(MaxNode) 'sankar
		'PollNode = 3
		'UPGRADE_WARNING: Couldn't resolve default property of object PollNode. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		Node(PollNode, 10) = 1
		'UPGRADE_WARNING: Couldn't resolve default property of object PollNode. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		EnumerateList(Str(PollNode) & ",")
		
		PaintGraph()
		'enumeration till the entire graph  to be completed
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Node(i, 10) <> 1 Then
				
				
				
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				Node(i, 10) = 1
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				EnumerateList(Str(i) & ",")
				PaintGraph()
				GoTo ABC
			End If
		Next i
ABC: 
		'    For i = 1 To MaxNode
		'        If Node(i, 10) <> 1 Then
		'
		'
		'
		'
		'            Node(i, 10) = 1
		'            EnumerateList (Str(i) & ",")
		'            PaintGraph
		'            Exit Sub
		'        End If
		'    Next i
		
		
	End Sub
	Public Function PaintGraph() As Object
		Dim i As Object
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Node(i, 5) = 1 Then
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_ISSUE: PictureBox method Picture1.Line was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
				Picture1.Line (Node(i, 1), Node(i, 2)) - (Node(Hello(i, 1), 1), Node(Hello(i, 1), 2)), System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Lime)
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			ElseIf Node(i, 5) = 2 Then 
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_ISSUE: PictureBox method Picture1.Line was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
				Picture1.Line (Node(i, 1), Node(i, 2)) - (Node(Hello(i, 1), 1), Node(Hello(i, 1), 2)), System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Lime)
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_ISSUE: PictureBox method Picture1.Line was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
				Picture1.Line (Node(i, 1), Node(i, 2)) - (Node(Hello(i, 2), 1), Node(Hello(i, 2), 2)), System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Lime)
			End If
		Next i
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Hello(i, 3) <> 0 Then
				'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
				'UPGRADE_ISSUE: PictureBox method Picture1.Line was not upgraded. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="CC4C7EC0-C903-48FC-ACCC-81861D12DA4A"'
				Picture1.Line (Node(i, 1), Node(i, 2)) - (Node(Hello(i, 3), 1), Node(Hello(i, 3), 2)), System.Drawing.ColorTranslator.ToOle(System.Drawing.Color.Blue)
			End If
		Next i
	End Function
	
	Public Sub Command4_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command4.Click
		Dim i As Object
		'' sankar This should be used for real time implimentation deletion
		''Way  1
		'    For i = 1 To MaxNode
		'        If Node(i, 10) = 0 Then
		'            MsgBox i & "USing Way 1"
		'        End If
		'    Next i
		
		'Way  2
		Dim Cnt As Short
		Cnt = 0
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Hello(i, 1) = 0 And Hello(i, 2) = 0 And Hello(i, 3) = 0 And Hello(i, 4) = 0 And Hello(i, 5) = 0 And Node(i, 3) <> 0 And Node(i, 10) <> False Then
				Label1(i).BackColor = System.Drawing.Color.Blue
				'MsgBox i & " Is not Connected to the network"
				Cnt = Cnt + 1
			End If
		Next i
		If Cnt >= 2 Then
			'MsgBox "The number of nodes deleted were" & NdeDeleted & " and " & Cnt & " Nodes were not Connected to the network"
			Form2.Text1.Text = Form2.Text1.Text & NdeDeleted & vbTab & Cnt & vbTab & Text1.Text & vbCrLf
			Flag = False
		End If
	End Sub
	
	Private Sub Command5_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command5.Click
		Dim i As Object
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			If Node(i, 5) = 1 Then
				MsgBox(i)
			End If
		Next i
	End Sub
	
	Private Sub Command6_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command6.Click
		Command4.Enabled = True
		Command5.Enabled = True
		Command7.Enabled = True
		Command11.Focus()
		DisplayComputer()
	End Sub
	
	
	
	
	
	Private Sub Command7_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command7.Click
		Dim Temp As Object
		Dim k As Short
		k = 0
		'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		Temp = GetRnd(100)
		'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		While Node(Temp, 10) <> 1 And k <= 100
			'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Temp = GetRnd(100)
			k = k + 1
		End While
		'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		If Node(Temp, 10) = 1 Then
			NdeDeleted = NdeDeleted + 1
			'UPGRADE_WARNING: Couldn't resolve default property of object Temp. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			DeleteNode(Temp)
		End If
		
		'MsgBox "The number of nodes deleted where" & NdeDeleted
	End Sub
	
	Private Sub Command8_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command8.Click
		Dim z As Object
		'UPGRADE_WARNING: Couldn't resolve default property of object z. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		z = GetRnd(1000)
		Text6.Text = CStr(Val(Text6.Text) + 1)
	End Sub
	
	Private Sub Command9_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command9.Click
		
		Command10.Focus()
		Flag = True
		Dim j As Short
		j = 0
		While (Flag = True And j <= 100)
			Command7_Click(Command7, New System.EventArgs())
			j = j + 1
		End While
	End Sub
	
	Private Sub Form1_Load(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles MyBase.Load
		Form2.Show()
		Text2.Text = InputBox("Enter the number of nodes")
		If Text2.Text = "" Then
			Text2.Text = CStr(100)
		End If
		MaxNode = Val(Text2.Text)
		ReDim Node(MaxNode, MaxNode)
		ReDim Hello(MaxNode, 5)
		NodeID = 0
		
	End Sub
	
	Private Sub Label1_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Label1.Click
		Dim Index As Short = Label1.GetIndex(eventSender)
		Dim ch As Object
		MsgBox("IndexID:" & Index & vbCrLf & "NodeID:" & Node(Index, 0) & vbCrLf & "(X,Y):" & "(" & Node(Index, 1) & "," & Node(Index, 2) & ")" & vbCrLf & "Power:" & Node(Index, 3) & vbCrLf)
		'UPGRADE_WARNING: Couldn't resolve default property of object ch. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		ch = MsgBox("Do you Want to Delete this node", MsgBoxStyle.YesNo)
		'UPGRADE_WARNING: Couldn't resolve default property of object ch. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
		If ch = 6 Then
			DeleteNode(Index)
		End If
	End Sub
	Public Function DisplayComputer() As Object
		Dim i As Object
		For i = 1 To MaxNode
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Label1(i).Text = i
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Label1(i).Left = VB6.TwipsToPixelsX(Node(i, 1))
			'UPGRADE_WARNING: Couldn't resolve default property of object i. Click for more: 'ms-help://MS.VSCC.v90/dv_commoner/local/redirect.htm?keyword="6A50421D-15FE-4896-8A1B-2EC21E9037B2"'
			Label1(i).Top = VB6.TwipsToPixelsY(Node(i, 2))
			Label1(i).Visible = True
		Next i
	End Function
End Class