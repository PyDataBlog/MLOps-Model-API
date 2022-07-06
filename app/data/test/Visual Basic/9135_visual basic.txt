Imports System.Linq
Imports Newtonsoft.Json

Public Class TypeScriptStubGenerator

#Region "Constants ..."

    Public Const INDENTATION As String = "    "

#End Region

#Region "Members ..."

    Private Property MetadataUrl As String
    Private Property OutputFolder As String
    Public Property Messages As New List(Of String)

#End Region

#Region "Enums ..."

    Public Enum enumNavigationPropertyType
        [Single] = 1
        Multiple = 2
    End Enum

#End Region

#Region "Constructor ..."

    Public Sub New(strMetadataUrl As String, strOutputFolder As String)
        Me.MetadataUrl = strMetadataUrl
        If Not strOutputFolder.EndsWith("\") Then strOutputFolder &= "\"
        Me.OutputFolder = strOutputFolder
    End Sub

#End Region

#Region "Public functions ..."

    Public Sub Generate()

        'Get the metadata ...
        Dim breezeMetadata As BreezeMetadata = GetMetadata()

        'Loop through all the entity types ..
        For Each breezeEntityType As BreezeEntityType In breezeMetadata.schema.entityType

            'Generate the stub code for the entity type ...
            Dim strStubCode As String = GenerateStub(breezeEntityType, breezeMetadata)

            'Write the stub file to the output directory ...
            WriteStubToFile(strStubCode, breezeEntityType)
        Next
    End Sub

#End Region

#Region "Stub generation methods ..."

    Private Function GenerateStub(breezeEntityType As BreezeEntityType, breezeMetadata As BreezeMetadata) As String
        Dim stringBuilder As New System.Text.StringBuilder

        'When we have any navigation properties ...
        If breezeEntityType.navigationProperties.Count > 0 Then

            'Loop though all the properties ...
            For Each breezeEntityTypeNavigationProperty As BreezeEntityTypeNavigationProperty In breezeEntityType.navigationProperties

                'Generate an import statement ...
                stringBuilder.AppendLine("import {" & breezeEntityTypeNavigationProperty.toRole & "} from './" & breezeEntityTypeNavigationProperty.toRole & "'")
            Next

            'Add an empty line ...
            stringBuilder.AppendLine("")
        End If

        'Export the class ...
        stringBuilder.AppendLine("export class " & breezeEntityType.name & " {")

        'When the entity has any properties ...
        If breezeEntityType.property.Count > 0 Then

            'Loop though all the properties ...
            For Each breezeEntityTypeProperty As BreezeEntityTypeProperty In breezeEntityType.property

                'Constructor ...
                stringBuilder.AppendLine(INDENTATION & breezeEntityTypeProperty.name & ": " & BreezePropertyTypeToTypeScriptType(breezeEntityTypeProperty.type) & ";")
            Next

            'Add an empty line ...
            stringBuilder.AppendLine("")
        End If

        'When we have any navigation properties ...
        If breezeEntityType.navigationProperties.Count > 0 Then

            'Loop though all the properties ...
            For Each breezeEntityTypeNavigationProperty As BreezeEntityTypeNavigationProperty In breezeEntityType.navigationProperties

                'Depending on the navigation property type ....
                Select Case GetNavigationPropertyType(breezeEntityTypeNavigationProperty, breezeMetadata)
                    Case enumNavigationPropertyType.Single
                        stringBuilder.AppendLine(INDENTATION & breezeEntityTypeNavigationProperty.name & ": " & breezeEntityTypeNavigationProperty.toRole & ";")
                    Case enumNavigationPropertyType.Multiple
                        stringBuilder.AppendLine(INDENTATION & breezeEntityTypeNavigationProperty.name & ": " & breezeEntityTypeNavigationProperty.toRole & "[];")
                End Select
            Next

            'Add an empty line ...
            stringBuilder.AppendLine("")
        End If

        'Constructor ...
        stringBuilder.AppendLine(INDENTATION & "constructor() { }")

        'End the class ...
        stringBuilder.AppendLine("}")

        Return stringBuilder.ToString
    End Function

    Private Sub WriteStubToFile(stubCode As String, breezeEntityType As BreezeEntityType)
        Dim stubFilePath As String

        'Build the stub file path ...
        stubFilePath = Me.OutputFolder & breezeEntityType.name & ".ts"

        'Write the file to the output path ...
        System.IO.File.WriteAllText(stubFilePath, stubCode)
    End Sub

#End Region

#Region "Metadata helper functions ..."

    Private Function GetMetadata() As BreezeMetadata
        Dim dataString As String = ""
        Dim breezeMetadata As BreezeMetadata = Nothing

        Try
            'Create a new web client ...
            Dim webClient As New System.Net.WebClient()

            'Download the data ...
            Dim data As Byte() = webClient.DownloadData(Me.MetadataUrl)

            'Read as a string ...
            dataString = System.Text.Encoding.UTF8.GetString(data)

        Catch ex As Exception
            Me.Messages.Add("Unable to obtain meta data. Error: " & ex.Message)
            Return Nothing
        End Try

        'Deserialize the json ...
        Try
            breezeMetadata = JsonConvert.DeserializeObject(Of BreezeMetadata)(dataString)
        Catch ex As Exception
            Me.Messages.Add("Unable to obtain meta data, invalid json. Error: " & ex.Message)
            Return Nothing
        End Try

        Return breezeMetadata
    End Function

    Private Function RemoveStringFromStart(inputString As String, strToRemove As String) As String

        'Remove strToRemove from the start when there ...
        If inputString.StartsWith(strToRemove) Then inputString = inputString.Substring(strToRemove.Length)
        Return inputString
    End Function

    Private Function RemovePrefrixFromRelationship(inputString As String, breezeMetadata As BreezeMetadata) As String

        'Remove "Self." from the start when there ...
        inputString = RemoveStringFromStart(inputString, "Self.")

        'Remove breezeMetadata.schema.namespace from the start when there ...
        inputString = RemoveStringFromStart(inputString, breezeMetadata.schema.namespace & ".")

        Return inputString
    End Function

    Private Function GetNavigationPropertyType(breezeEntityTypeNavigationProperty As BreezeEntityTypeNavigationProperty, breezeMetadata As BreezeMetadata) As enumNavigationPropertyType

        'Loop through all the associations ...
        For Each breezeAssociation As BreezeAssociation In breezeMetadata.schema.association
            If breezeAssociation.name = RemovePrefrixFromRelationship(breezeEntityTypeNavigationProperty.relationship, breezeMetadata) Then

                'Loop though all the ends of the association ...
                For Each breezeAssociationEnd As BreezeAssociationEnd In breezeAssociation.ends

                    'When the end for the toRole ...
                    If breezeAssociationEnd.role = breezeEntityTypeNavigationProperty.toRole Then
                        Select Case breezeAssociationEnd.multiplicity
                            Case "*"
                                Return enumNavigationPropertyType.Multiple
                            Case "1", "0..1"
                                Return enumNavigationPropertyType.Single
                            Case Else
                                Throw New Exception("Invalid multiplicity value: """ & breezeAssociationEnd.multiplicity & """ on association """ & breezeAssociation.name & """")
                        End Select
                    End If
                Next
            End If
        Next

        'Not found - Throw an exception ... 
        Throw New Exception("Unable to determine navigation property multiplicty type on association """ & breezeEntityTypeNavigationProperty.relationship & """ for toRole """ & breezeEntityTypeNavigationProperty.toRole & """")
    End Function

    Private Function BreezePropertyTypeToTypeScriptType(strBreezePropertyType As String) As String

        'The edm types are documented here https://msdn.microsoft.com/en-us/library/ee382832(v=vs.100).aspx
        'Convert edm types to target typescript types ..
        Select Case strBreezePropertyType
            Case "Edm.String", "Edm.Guid"
                Return "String"
            Case "Edm.Boolean"
                Return "boolean"
            Case "Edm.Int16", "Edm.Int32", "Edm.Int64", "Edm.Float", "Edm.Double", "Edm.Decimal", "Edm.Byte", "Edm.SByte"
                Return "Number"
            Case "Edm.DateTime", "Edm.Time"
                Return "Date"
            Case "Edm.Binary", "Edm.DateTimeOffset"
                Return "any"
            Case Else
                Return "any"
        End Select
    End Function

#End Region

End Class
