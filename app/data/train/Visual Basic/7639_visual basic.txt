'ApplicationLauncher.vb
'
'LuminousForts Toolset -- A modders toolkit, written in VB.Net
'Copyright (C) 2010 Hekar Khani

'This program is free software: you can redistribute it and/or modify
'it under the terms of the GNU General Public License as published by
'the Free Software Foundation, either version 3 of the License, or
'(at your option) any later version.

'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.

'You should have received a copy of the GNU General Public License
'along with this program.  If not, see <http://www.gnu.org/licenses/>.

Imports L_Shared
Imports System.IO.MemoryMappedFiles

' Launches and sets up shared memory with an application
Public Class ApplicationLauncher

    ' Process info
    Private _procinfo As ProcessStartInfo
    Private _process As Process

    ' Setup shared memory, etc
    Private _memorymappedfile As MemoryMappedFile

    ' Filename is filename and not filepath
    Public Sub New(ByRef filename As String, ByRef workingdir As String)

        ' Set the process info
        _procinfo = New ProcessStartInfo()
        _procinfo.FileName = workingdir & "\" & filename
        _procinfo.WorkingDirectory = workingdir

        _memorymappedfile = MemoryMappedFile.CreateOrOpen(ModShared.kModMemoryMapping, 5012)

    End Sub

    Public Sub WaitForExit()
        _process.WaitForExit()
    End Sub

    Public Sub Start()
        _process = Process.Start(_procinfo)
    End Sub

End Class
