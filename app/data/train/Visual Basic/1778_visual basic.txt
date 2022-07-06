Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace ARPrimaryQuery.CustomerQuery.QueryData
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class Key
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private customerField As String
        
        '''<summary>
        '''The customer for which the statement is required
        '''</summary>
        Public Property Customer() As String
            Get
                Return Me.customerField
            End Get
            Set
                Me.customerField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class [Option]
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private multiMediaImageTypeField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeFuturesField As IncludeFutures
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeFuturesFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeTransactionsField As IncludeTransactions
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeTransactionsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCheckPaymentsField As IncludeCheckPayments
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCheckPaymentsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includePostDatedField As IncludePostDated
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includePostDatedFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeZeroBalancesField As IncludeZeroBalances
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeZeroBalancesFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCustomFormsField As IncludeCustomForms
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private includeCustomFormsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private asOfPeriodField As AsOfPeriod
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private asOfPeriodFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageingOptionField As OptionAgeingOption
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageingOptionFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn1Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn2Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn3Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn4Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn5Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn6Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ageColumn7Field As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private xslStylesheetField As String
        
        Public Sub New()
            MyBase.New
            Me.includeFuturesField = IncludeFutures.N
            Me.includeTransactionsField = IncludeTransactions.Y
            Me.includeCheckPaymentsField = IncludeCheckPayments.Y
            Me.includePostDatedField = IncludePostDated.Y
            Me.includeZeroBalancesField = IncludeZeroBalances.N
            Me.includeCustomFormsField = IncludeCustomForms.N
            Me.asOfPeriodField = AsOfPeriod.C
        End Sub
        
        '''<summary>
        '''Image type to retrieve (for example: GIF, JPG)
        '''</summary>
        Public Property MultiMediaImageType() As String
            Get
                Return Me.multiMediaImageTypeField
            End Get
            Set
                Me.multiMediaImageTypeField = value
            End Set
        End Property
        
        '''<summary>
        '''Do you wish to include future invoices ? (Y - Yes, N - No))
        '''</summary>
        Public Property IncludeFutures() As IncludeFutures
            Get
                Return Me.includeFuturesField
            End Get
            Set
                Me.includeFuturesField = value
                Me.IncludeFuturesFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeFuturesSpecified() As Boolean
            Get
                Return Me.includeFuturesFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''Do you wish to include invoice transaction details ? (Y - Yes, N - No))
        '''</summary>
        Public Property IncludeTransactions() As IncludeTransactions
            Get
                Return Me.includeTransactionsField
            End Get
            Set
                Me.includeTransactionsField = value
                Me.IncludeTransactionsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeTransactionsSpecified() As Boolean
            Get
                Return Me.includeTransactionsFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''Do you wish to include check payment details ? (Y - Yes, N - No))
        '''</summary>
        Public Property IncludeCheckPayments() As IncludeCheckPayments
            Get
                Return Me.includeCheckPaymentsField
            End Get
            Set
                Me.includeCheckPaymentsField = value
                Me.IncludeCheckPaymentsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeCheckPaymentsSpecified() As Boolean
            Get
                Return Me.includeCheckPaymentsFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''Do you wish to include post dated check details ? (Y - Yes, N - No))
        '''</summary>
        Public Property IncludePostDated() As IncludePostDated
            Get
                Return Me.includePostDatedField
            End Get
            Set
                Me.includePostDatedField = value
                Me.IncludePostDatedFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludePostDatedSpecified() As Boolean
            Get
                Return Me.includePostDatedFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''Do you wish to include invoices with a zero balance ? (Y - Yes, N - No))
        '''</summary>
        Public Property IncludeZeroBalances() As IncludeZeroBalances
            Get
                Return Me.includeZeroBalancesField
            End Get
            Set
                Me.includeZeroBalancesField = value
                Me.IncludeZeroBalancesFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeZeroBalancesSpecified() As Boolean
            Get
                Return Me.includeZeroBalancesFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''Do you wish to include fields from custom forms ? (N - No, Y - Yes,). Default is N - No.
        '''</summary>
        Public Property IncludeCustomForms() As IncludeCustomForms
            Get
                Return Me.includeCustomFormsField
            End Get
            Set
                Me.includeCustomFormsField = value
                Me.IncludeCustomFormsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IncludeCustomFormsSpecified() As Boolean
            Get
                Return Me.includeCustomFormsFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''This element specifies the period from which to return data (C - current period backwards, P - the previous period backwards, 2 -  previous period two backwards). If this element is not provided the default is C.
        '''</summary>
        Public Property AsOfPeriod() As AsOfPeriod
            Get
                Return Me.asOfPeriodField
            End Get
            Set
                Me.asOfPeriodField = value
                Me.AsOfPeriodFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property AsOfPeriodSpecified() As Boolean
            Get
                Return Me.asOfPeriodFieldSpecified
            End Get
        End Property
        
        Public Property AgeingOption() As OptionAgeingOption
            Get
                Return Me.ageingOptionField
            End Get
            Set
                Me.ageingOptionField = value
                Me.AgeingOptionFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property AgeingOptionSpecified() As Boolean
            Get
                Return Me.ageingOptionFieldSpecified
            End Get
        End Property
        
        Public Property AgeColumn1() As String
            Get
                Return Me.ageColumn1Field
            End Get
            Set
                Me.ageColumn1Field = value
            End Set
        End Property
        
        Public Property AgeColumn2() As String
            Get
                Return Me.ageColumn2Field
            End Get
            Set
                Me.ageColumn2Field = value
            End Set
        End Property
        
        Public Property AgeColumn3() As String
            Get
                Return Me.ageColumn3Field
            End Get
            Set
                Me.ageColumn3Field = value
            End Set
        End Property
        
        Public Property AgeColumn4() As String
            Get
                Return Me.ageColumn4Field
            End Get
            Set
                Me.ageColumn4Field = value
            End Set
        End Property
        
        Public Property AgeColumn5() As String
            Get
                Return Me.ageColumn5Field
            End Get
            Set
                Me.ageColumn5Field = value
            End Set
        End Property
        
        Public Property AgeColumn6() As String
            Get
                Return Me.ageColumn6Field
            End Get
            Set
                Me.ageColumn6Field = value
            End Set
        End Property
        
        Public Property AgeColumn7() As String
            Get
                Return Me.ageColumn7Field
            End Get
            Set
                Me.ageColumn7Field = value
            End Set
        End Property
        
        Public Property XslStylesheet() As String
            Get
                Return Me.xslStylesheetField
            End Get
            Set
                Me.xslStylesheetField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludeFutures
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludeTransactions
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludeCheckPayments
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludePostDated
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludeZeroBalances
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IncludeCustomForms
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum AsOfPeriod
        
        '''<remarks/>
        C
        
        '''<remarks/>
        P
        
        '''<remarks/>
        <System.Xml.Serialization.XmlEnumAttribute("2")>  _
        Item2
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum OptionAgeingOption
        
        '''<remarks/>
        S
        
        '''<remarks/>
        A
        
        '''<remarks/>
        I
        
        '''<remarks/>
        D
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class Query
        Implements Codewell.SysproIntegration.IQuery
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private keyField As Key
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private optionField As [Option]
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _queryActionTypeField As CommonEnums.ActionType_Query
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private _xmlOut As String
        
        Public Sub New()
            MyBase.New
            If (Me.optionField Is Nothing) Then
                Me.optionField = New [Option]
            End If
            If (Me.keyField Is Nothing) Then
                Me.keyField = New Key
            End If
        End Sub
        
        Public Property Key() As Key
            Get
                Return Me.keyField
            End Get
            Set
                Me.keyField = value
            End Set
        End Property
        
        Public Property [Option]() As [Option]
            Get
                Return Me.optionField
            End Get
            Set
                Me.optionField = value
            End Set
        End Property
        
        Public Overridable Property QueryActionType() As CommonEnums.ActionType_Query Implements IQuery.QueryActionType
            Get
                Return _queryActionTypeField
            End Get
            Set
                Me._queryActionTypeField = value
            End Set
        End Property
        
        Public Overridable ReadOnly Property XmlOut() As String Implements IQuery.XmlOut
            Get
                Return _xmlOut
            End Get
        End Property
        
        Public Overridable ReadOnly Property BusinessObject() As String Implements IQuery.BusinessObject
            Get
                Return "ARSQRY"
            End Get
        End Property
        
        Public Overridable ReadOnly Property XMLData() As String Implements IQuery.XMLData
            Get
                Dim ms As System.IO.MemoryStream
                Dim s As System.Xml.Serialization.XmlSerializer
                Dim tw As System.Xml.XmlTextWriter
                Dim doc As System.Xml.XmlDocument
                ms = New System.IO.MemoryStream
                s = New System.Xml.Serialization.XmlSerializer(Me.GetType)
                tw = New System.Xml.XmlTextWriter(ms, Nothing)
                doc = New System.Xml.XmlDocument
                s.Serialize(tw, Me)
                ms.Seek(0, 0)
                doc.Load(ms)
                Return doc.OuterXml
            End Get
        End Property
        
        Public Overridable Sub Post(ByVal blnThrowExceptionOnErrorInXMLOut As Boolean) Implements IQuery.Post
            If (Me._queryActionTypeField = 1) Then
                Me._xmlOut = QueryObject.Browse(Me)
                Return
            End If
            If (Me._queryActionTypeField = 0) Then
                Me._xmlOut = QueryObject.Fetch(Me)
                Return
            End If
            If (Me._queryActionTypeField = 4) Then
                Me._xmlOut = QueryObject.Query(Me)
                Return
            End If
            If (Me._queryActionTypeField = 2) Then
                Me._xmlOut = QueryObject.NextKey(Me)
                Return
            End If
            If (Me._queryActionTypeField = 3) Then
                Me._xmlOut = QueryObject.PreviousKey(Me)
                Return
            End If
            If blnThrowExceptionOnErrorInXMLOut Then
                Common.SysproUtilities.CheckXMLOut(XMLData, "", XmlOut)
            End If
        End Sub
    End Class
End Namespace
