Del /f /s /q "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\*"
Del /f /s /q "C:\Users\Joseph\Documents\Companies\Postah\Surf\Upload\surf-windows.zip"

xcopy /e /y /v "C:\Users\Joseph\Documents\Companies\Postah\Surf\Surf\Surf\bin\Release" "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\"

Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\surf.vshost.exe"
Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\surf.pdb"
Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\surf.vshost.exe.manifest"
Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\surf.vshost.exe.config"
Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\surf.xml"
Del "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\*tmp"

"C:\Program Files\WinRAR\winrar.exe" a -afzip -m5 -ed -r -ep1 -ibck "C:\Users\Joseph\Documents\Companies\Postah\Surf\Upload\surf-windows.zip" "C:\Users\Joseph\Documents\Companies\Postah\Surf\OutgoingBuild\*"