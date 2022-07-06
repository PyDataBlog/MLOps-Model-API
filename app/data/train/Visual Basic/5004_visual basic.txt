Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace InventoryPrimaryPosting.InventoryMovementsCostModifications.Params
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class Parameters
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private transactionDateField As String
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignoreWarningsField As IgnoreWarnings
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignoreWarningsFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private applyIfEntireDocumentValidField As ApplyIfEntireDocumentValid
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private applyIfEntireDocumentValidFieldSpecified As Boolean
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private validateOnlyField As ValidateOnly
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private validateOnlyFieldSpecified As Boolean
        
        Public Sub New()
            MyBase.New
            Me.ignoreWarningsField = IgnoreWarnings.N
            Me.applyIfEntireDocumentValidField = ApplyIfEntireDocumentValid.Y
            Me.validateOnlyField = ValidateOnly.N
        End Sub
        
        '''<summary>
        '''Date the transaction raised. If not supplied rundate is assumed.
        '''</summary>
        Public Property TransactionDate() As String
            Get
                Return Me.transactionDateField
            End Get
            Set
                Me.transactionDateField = value
            End Set
        End Property
        
        '''<summary>
        '''This element indicates whether or not a transaction must be rejected if there are any warnings such as cost variance exceeded or zero cost supplied. IF 'W' is supplied, the errors will be reported as warnings. If not supplied it defaults to 'N'. Valid values are 'N', 'W' and 'Y.
        '''</summary>
        Public Property IgnoreWarnings() As IgnoreWarnings
            Get
                Return Me.ignoreWarningsField
            End Get
            Set
                Me.ignoreWarningsField = value
                Me.IgnoreWarningsFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property IgnoreWarningsSpecified() As Boolean
            Get
                Return Me.ignoreWarningsFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''This element indicates whether to process entire document only if all items are valid. If set to 'N', each item is validated and rejected if in error or processed if correct. This is the fastest option. If set to 'Y', all items are validated and only processed if entire document is valid. Default is 'Y'. Valid values are 'Y' and 'N'.
        '''
        '''</summary>
        Public Property ApplyIfEntireDocumentValid() As ApplyIfEntireDocumentValid
            Get
                Return Me.applyIfEntireDocumentValidField
            End Get
            Set
                Me.applyIfEntireDocumentValidField = value
                Me.ApplyIfEntireDocumentValidFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property ApplyIfEntireDocumentValidSpecified() As Boolean
            Get
                Return Me.applyIfEntireDocumentValidFieldSpecified
            End Get
        End Property
        
        '''<summary>
        '''This element indicates which processing phases are to be performed. If set to 'Y' all items are validated and an XML string is returned with the result of the validation. No updating will take place. If set to 'N', each item is validated and updating is performed based on setting of the element 'ApplyIfEntireDocumentValid'. Default is 'N'. Valid values are 'N' and 'Y'.
        '''
        '''</summary>
        Public Property ValidateOnly() As ValidateOnly
            Get
                Return Me.validateOnlyField
            End Get
            Set
                Me.validateOnlyField = value
                Me.ValidateOnlyFieldSpecified = true
            End Set
        End Property
        
        '''<remarks/>
        <System.Xml.Serialization.XmlIgnoreAttribute()>  _
        Public ReadOnly Property ValidateOnlySpecified() As Boolean
            Get
                Return Me.validateOnlyFieldSpecified
            End Get
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum IgnoreWarnings
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        W
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Public Enum ApplyIfEntireDocumentValid
        
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
    Public Enum ValidateOnly
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class PostInvCostModification
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parametersField As Parameters
        
        Public Sub New()
            MyBase.New
            If (Me.parametersField Is Nothing) Then
                Me.parametersField = New Parameters
            End If
        End Sub
        
        '''<summary>
        '''This element contains parameters.
        '''</summary>
        Public Property Parameters() As Parameters
            Get
                Return Me.parametersField
            End Get
            Set
                Me.parametersField = value
            End Set
        End Property
    End Class
End Namespace
