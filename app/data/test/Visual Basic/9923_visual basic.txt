Imports System
Imports System.Diagnostics
Imports System.Collections.Generic
Imports System.Xml.Serialization
Imports System.Collections
Imports System.Xml.Schema
Imports System.ComponentModel

Namespace SystemSetup.CustomFormSetup.Params
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Diagnostics.DebuggerStepThroughAttribute(),  _
     System.ComponentModel.DesignerCategoryAttribute("code"),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true),  _
     System.Xml.Serialization.XmlRootAttribute([Namespace]:="", IsNullable:=false)>  _
    Partial Public Class SetupCustomForm
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private parametersField As SetupCustomFormParameters
        
        Public Sub New()
            MyBase.New
            If (Me.parametersField Is Nothing) Then
                Me.parametersField = New SetupCustomFormParameters
            End If
        End Sub
        
        Public Property Parameters() As SetupCustomFormParameters
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
    Partial Public Class SetupCustomFormParameters
        
        <EditorBrowsable(EditorBrowsableState.Never)>  _
        Private validateOnlyField As SetupCustomFormParametersValidateOnly
        
        Public Sub New()
            MyBase.New
            Me.validateOnlyField = SetupCustomFormParametersValidateOnly.Y
        End Sub
        
        Public Property ValidateOnly() As SetupCustomFormParametersValidateOnly
            Get
                Return Me.validateOnlyField
            End Get
            Set(ByVal value As SetupCustomFormParametersValidateOnly)
                Me.validateOnlyField = Value
            End Set
        End Property
    End Class
    
    '''<remarks/>
    <System.CodeDom.Compiler.GeneratedCodeAttribute("Shell", "1.0.0.0"),  _
     System.SerializableAttribute(),  _
     System.Xml.Serialization.XmlTypeAttribute(AnonymousType:=true)>  _
    Public Enum SetupCustomFormParametersValidateOnly
        
        '''<remarks/>
        Y
        
        '''<remarks/>
        N
    End Enum
End Namespace
