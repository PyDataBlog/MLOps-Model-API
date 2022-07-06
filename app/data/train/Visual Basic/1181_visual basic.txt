Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace WipPrimaryPosting.TransferWorkInProgress.Params
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class PostWipTransfer
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parametersField As PostWipTransferParameters
        
        Public Sub New()
            MyBase.New
            If (Me.parametersField Is Nothing) Then
                Me.parametersField = New PostWipTransferParameters
            End If
        End Sub
        
        Public Property Parameters() As PostWipTransferParameters
            Get
                Return Me.parametersField
            End Get
            Set
                Me.parametersField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Partial Public Class PostWipTransferParameters
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private validateOnlyField As PostWipTransferParametersValidateOnly
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private applyIfEntireDocumentValidField As PostWipTransferParametersApplyIfEntireDocumentValid
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignoreWarningsField As PostWipTransferParametersIgnoreWarnings
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionDateField As Date
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionDateFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private nonStockedWhToUseField As String
        
        Public Sub New()
            MyBase.New
            Me.validateOnlyField = PostWipTransferParametersValidateOnly.N
            Me.applyIfEntireDocumentValidField = PostWipTransferParametersApplyIfEntireDocumentValid.N
            Me.ignoreWarningsField = PostWipTransferParametersIgnoreWarnings.N
        End Sub
        
        <System.ComponentModel.DefaultValueAttribute(PostWipTransferParametersValidateOnly.N)>  _
        Public Property ValidateOnly() As PostWipTransferParametersValidateOnly
            Get
                Return Me.validateOnlyField
            End Get
            Set
                Me.validateOnlyField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWipTransferParametersApplyIfEntireDocumentValid.N)>  _
        Public Property ApplyIfEntireDocumentValid() As PostWipTransferParametersApplyIfEntireDocumentValid
            Get
                Return Me.applyIfEntireDocumentValidField
            End Get
            Set
                Me.applyIfEntireDocumentValidField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWipTransferParametersIgnoreWarnings.N)>  _
        Public Property IgnoreWarnings() As PostWipTransferParametersIgnoreWarnings
            Get
                Return Me.ignoreWarningsField
            End Get
            Set
                Me.ignoreWarningsField = value
            End Set
        End Property
        
        <System.Xml.Serialization.XmlElementAttribute(DataType:="date")>  _
        Public Property TransactionDate() As Date
            Get
                Return Me.transactionDateField
            End Get
            Set
                Me.transactionDateField = value
                Me.TransactionDateFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property TransactionDateSpecified() As Boolean
            Get
                Return Me.transactionDateFieldSpecified
            End Get
        End Property
        
        Public Property NonStockedWhToUse() As String
            Get
                Return Me.nonStockedWhToUseField
            End Get
            Set
                Me.nonStockedWhToUseField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWipTransferParametersValidateOnly
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWipTransferParametersApplyIfEntireDocumentValid
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWipTransferParametersIgnoreWarnings
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
End Namespace
