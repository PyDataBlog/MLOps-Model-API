Imports Aspose.Cloud

Namespace Document.SplitResources
    Class SplitToNewPDFs
        Public Shared Sub Run()
            Dim inputfile As String = "doc-sample.doc"

            ' Upload input file from local directory to Cloud Storage
            Common.UploadFile(inputfile, String.Empty)

            ' Split all Pages to new PDFs
            Dim wordsSplitResultResponse As WordsSplitResultResponse = Common.WordsService.SplitDocument(inputfile, SplitDocumentFormat.Pdf, 1, 2, Common.FOLDER)

            Console.WriteLine((Convert.ToString(vbLf) & inputfile) + " splitted successfully to new PDFs")
        End Sub
    End Class
End Namespace