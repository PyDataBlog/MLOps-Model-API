Imports System.Net
Imports System.IO
Imports System.Xml
Imports System.Text
Imports ConsoleApplication1.MoceanNamespace

Namespace MoceanNamespace
    Public Class MoceanSMSClass
        Public Function SendSMS(ByVal mocean_api_key As String, ByVal mocean_api_secret As String,
                         ByVal mocean_from As String, ByVal mocean_to As String,
                         ByVal mocean_text As String) As Hashtable

            Using client As New Net.WebClient
                Dim reqparm As New Specialized.NameValueCollection
                Dim sUrl As String = "https://rest-api.moceansms.com/rest/1/sms"

                reqparm.Add("mocean-api-key", mocean_api_key)
                reqparm.Add("mocean-api-secret", mocean_api_secret)
                reqparm.Add("mocean-from", mocean_from)
                reqparm.Add("mocean-to", mocean_to)
                reqparm.Add("mocean-text", mocean_text)

                Dim responsebytes = client.UploadValues(sUrl, "POST", reqparm)
                Dim responsebody = (New Text.UTF8Encoding).GetString(responsebytes)
                Dim MyHash As New Hashtable
                Using reader As XmlReader = XmlReader.Create(New StringReader(responsebody))
                    Dim xmlDoc As New XmlDocument
                    xmlDoc.Load(reader)

                    Dim root As XmlElement = xmlDoc.DocumentElement

                    MyHash.Add("status", root.GetElementsByTagName("status").Item(0).InnerText)
                    If MyHash("status") = 0 Then
                        MyHash.Add("receiver", root.GetElementsByTagName("receiver").Item(0).InnerText)
                        MyHash.Add("msgid", root.GetElementsByTagName("msgid").Item(0).InnerText)
                    Else
                        MyHash.Add("err_msg", root.GetElementsByTagName("err_msg").Item(0).InnerText)
                        If MyHash("status") = 2 Then
                            MyHash.Add("receiver", root.GetElementsByTagName("receiver").Item(0).InnerText)
                        End If
                    End If
                End Using

                Return MyHash
            End Using

        End Function
        Public Function CheckMoceanBalance(ByVal mocean_api_key As String, ByVal mocean_api_secret As String) As Hashtable

            Using client As New Net.WebClient
                Dim reqparm As New Specialized.NameValueCollection
                Dim sUrl As String = "https://rest-api.moceansms.com/rest/1/account/balance"
                Dim param As String = "?mocean-api-key=" + mocean_api_key + "&mocean-api-secret=" + mocean_api_secret

                Dim MyHash As New Hashtable

                Dim response = client.DownloadString(sUrl + param)
                Using reader As XmlReader = XmlReader.Create(New StringReader(response))
                    Dim xmlDoc As New XmlDocument
                    xmlDoc.Load(reader)

                    Dim root As XmlElement = xmlDoc.DocumentElement

                    MyHash.Add("status", root.GetElementsByTagName("status").Item(0).InnerText)
                    If MyHash("status") = 0 Then
                        MyHash.Add("value", root.GetElementsByTagName("value").Item(0).InnerText)
                    Else
                        MyHash.Add("err_msg", root.GetElementsByTagName("err_msg").Item(0).InnerText)
                    End If
                End Using


                Return MyHash
            End Using
        End Function
        Public Function MoceanAccountPricing(ByVal mocean_api_key As String, ByVal mocean_api_secret As String) As Hashtable

            Using client As New Net.WebClient
                Dim reqparm As New Specialized.NameValueCollection
                Dim sUrl As String = "https://rest-api.moceansms.com/rest/1/account/pricing"
                Dim param As String = "?mocean-api-key=" + mocean_api_key + "&mocean-api-secret=" + mocean_api_secret

                Dim MyHash As New Hashtable

                Dim response = client.DownloadString(sUrl + param)
                Using reader As XmlReader = XmlReader.Create(New StringReader(response))
                    Dim xmlDoc As New XmlDocument
                    xmlDoc.Load(reader)

                    Dim root As XmlElement = xmlDoc.DocumentElement
                    MyHash.Add("status", root.GetElementsByTagName("status").Item(0).InnerText)
                    If MyHash("status") = 0 Then
                        MyHash.Add("country", root.GetElementsByTagName("country").Item(0).InnerText)
                        MyHash.Add("operator", root.GetElementsByTagName("operator").Item(0).InnerText)
                        MyHash.Add("mcc", root.GetElementsByTagName("mcc").Item(0).InnerText)
                        MyHash.Add("mnc", root.GetElementsByTagName("mnc").Item(0).InnerText)
                        MyHash.Add("price", root.GetElementsByTagName("price").Item(0).InnerText)
                    Else
                        MyHash.Add("err_msg", root.GetElementsByTagName("err_msg").Item(0).InnerText)
                    End If

                End Using


                Return MyHash
            End Using
        End Function
        Public Function MoceanMessageStatus(ByVal mocean_api_key As String, ByVal mocean_api_secret As String, ByVal mocean_msgid As String) As Hashtable

            Using client As New Net.WebClient
                Dim reqparm As New Specialized.NameValueCollection
                Dim sUrl As String = "https://rest-api.moceansms.com/rest/1/report/message"
                Dim param As String = "?mocean-api-key=" + mocean_api_key + "&mocean-api-secret=" + mocean_api_secret + "&mocean-msgid=" + mocean_msgid

                Dim MyHash As New Hashtable

                Dim response = client.DownloadString(sUrl + param)
                Using reader As XmlReader = XmlReader.Create(New StringReader(response))
                    Dim xmlDoc As New XmlDocument
                    xmlDoc.Load(reader)

                    Dim root As XmlElement = xmlDoc.DocumentElement

                    MyHash.Add("status", root.GetElementsByTagName("status").Item(0).InnerText)
                    If MyHash("status") = 0 Then
                        MyHash.Add("msgid", root.GetElementsByTagName("msgid").Item(0).InnerText)
                        MyHash.Add("message_status", root.GetElementsByTagName("message_status").Item(0).InnerText)
                        If MyHash("message_status") = 2 Then
                            MyHash.Add("message_error_code", root.GetElementsByTagName("message_error_code").Item(0).InnerText)
                        End If
                    Else
                        MyHash.Add("err_msg", root.GetElementsByTagName("err_msg").Item(0).InnerText)
                    End If
                End Using


                Return MyHash
            End Using
        End Function
    End Class
End Namespace
