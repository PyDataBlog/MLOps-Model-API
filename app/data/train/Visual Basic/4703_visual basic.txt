Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace WipTwoStageReceipting.WipInspectionAcceptIntoStock.Params
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class PostWIPInspectAccept
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parametersField As PostWIPInspectAcceptParameters
        
        Public Sub New()
            MyBase.New
            If (Me.parametersField Is Nothing) Then
                Me.parametersField = New PostWIPInspectAcceptParameters
            End If
        End Sub
        
        Public Property Parameters() As PostWIPInspectAcceptParameters
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
    Partial Public Class PostWIPInspectAcceptParameters
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private validateOnlyField As PostWIPInspectAcceptParametersValidateOnly
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private ignoreWarningsField As PostWIPInspectAcceptParametersIgnoreWarnings
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private applyIfEntireDocumentValidField As PostWIPInspectAcceptParametersApplyIfEntireDocumentValid
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private manualSerialTransfersAllowedField As PostWIPInspectAcceptParametersManualSerialTransfersAllowed
        
        Public Sub New()
            MyBase.New
            Me.validateOnlyField = PostWIPInspectAcceptParametersValidateOnly.N
            Me.ignoreWarningsField = PostWIPInspectAcceptParametersIgnoreWarnings.Y
            Me.applyIfEntireDocumentValidField = PostWIPInspectAcceptParametersApplyIfEntireDocumentValid.N
            Me.manualSerialTransfersAllowedField = PostWIPInspectAcceptParametersManualSerialTransfersAllowed.N
        End Sub
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectAcceptParametersValidateOnly.N)>  _
        Public Property ValidateOnly() As PostWIPInspectAcceptParametersValidateOnly
            Get
                Return Me.validateOnlyField
            End Get
            Set
                Me.validateOnlyField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectAcceptParametersIgnoreWarnings.Y)>  _
        Public Property IgnoreWarnings() As PostWIPInspectAcceptParametersIgnoreWarnings
            Get
                Return Me.ignoreWarningsField
            End Get
            Set
                Me.ignoreWarningsField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectAcceptParametersApplyIfEntireDocumentValid.N)>  _
        Public Property ApplyIfEntireDocumentValid() As PostWIPInspectAcceptParametersApplyIfEntireDocumentValid
            Get
                Return Me.applyIfEntireDocumentValidField
            End Get
            Set
                Me.applyIfEntireDocumentValidField = value
            End Set
        End Property
        
        <System.ComponentModel.DefaultValueAttribute(PostWIPInspectAcceptParametersManualSerialTransfersAllowed.N)>  _
        Public Property ManualSerialTransfersAllowed() As PostWIPInspectAcceptParametersManualSerialTransfersAllowed
            Get
                Return Me.manualSerialTransfersAllowedField
            End Get
            Set
                Me.manualSerialTransfersAllowedField = value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectAcceptParametersValidateOnly
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectAcceptParametersIgnoreWarnings
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectAcceptParametersApplyIfEntireDocumentValid
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum PostWIPInspectAcceptParametersManualSerialTransfersAllowed
        
        '''<remarks/>
        N
        
        '''<remarks/>
        Y
    End Enum
End Namespace
