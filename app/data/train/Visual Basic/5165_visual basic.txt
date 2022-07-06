Imports Aspose.Cloud

Namespace Watermark
    Class AddImage
        Public Shared Sub Run()
            Dim inputfile As String = "doc-sample.doc"
            Dim imagefile As String = "Aspose Logo.png"

            Dim outfile As String = Common.GetOutputFilePath(inputfile, True)

            ' Upload input file from local directory to Cloud Storage
            Common.UploadFile(inputfile, String.Empty)

            ' Upload image file from local directory to Cloud Storage
            Common.UploadFile(imagefile, String.Empty)

            ' Add Watermark Image to a Word Document
            Common.WordsService.WordsDocumentWatermark.InsertDocumentWatermarkImage(inputfile, outfile, 45.0, imagefile, Common.FOLDER)

            ' Download output file from cloud storage and save on local directory
            Dim dataDir As String = Common.DownloadFile(inputfile, String.Empty)

            Console.WriteLine(Convert.ToString(vbLf & "Watermark image added successfully." & vbLf & "File saved at ") & dataDir)
        End Sub
    End Class
End Namespace
