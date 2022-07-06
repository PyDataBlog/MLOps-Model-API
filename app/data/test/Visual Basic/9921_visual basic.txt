Imports Aspose.Cloud

Namespace Hyperlink
    Class GetParticularHyperlink
        Public Shared Sub Run()
            Dim inputfile As String = "doc-sample-hyperlink.doc"

            ' Upload input file from local directory to Cloud Storage
            Common.UploadFile(inputfile, String.Empty)

            ' Get a Particular Hyperlink from a Word
            Dim wordsHyperlinkResponse As WordsHyperlinkResponse = Common.WordsService.WordsHyperlinks.ReadDocumentHyperlinkByItsIndex(inputfile, 0, Common.FOLDER)

            Console.WriteLine((Convert.ToString(vbLf) & inputfile) + " have " + wordsHyperlinkResponse.Hyperlink.DisplayText + " hyperlink")
        End Sub
    End Class
End Namespace