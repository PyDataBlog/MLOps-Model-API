.text:00401000 ; Format      : Portable executable for 80386 (PE)
.text:00401000 ; Imagebase   : 400000
.text:00401000 ; Section 1. (virtual address 00001000)
.text:00401000 ; Virtual size                  : 00001000 (   4096.)
.text:00401000 ; Section size in file          : 00000200 (    512.)
.text:00401000 ; Offset to raw data for section: 00000200
.text:00401000 ; Flags E0000060: Text Data Executable Readable Writable
.text:00401000 ; Alignment     : default
.text:00401000
.text:00401000                 .686p
.text:00401000                 .mmx
.text:00401000                 .model flat
.text:00401000
.text:00401000 ; ===========================================================================
.text:00401000
.text:00401000 ; Segment type: Pure code
.text:00401000 ; Segment permissions: Read/Write/Execute
.text:00401000 _text           segment para public 'CODE' use32
.text:00401000                 assume cs:_text
.text:00401000                 ;org 401000h
.text:00401000                 assume es:nothing, ss:nothing, ds:_data, fs:nothing, gs:nothing
.text:00401000
.text:00401000 ; =============== S U B R O U T I N E =======================================
.text:00401000
.text:00401000 ; Attributes: bp-based frame
.text:00401000
.text:00401000                 public start
.text:00401000 start           proc near
.text:00401000
.text:00401000 var_10          = dword ptr -10h
.text:00401000 var_C           = dword ptr -0Ch
.text:00401000 hFile           = dword ptr -8
.text:00401000 NumberOfBytesWritten= dword ptr -4
.text:00401000
.text:00401000                 push    ebp
.text:00401001                 mov     ebp, esp
.text:00401003                 sub     esp, 10h
.text:00401006                 mov     [ebp+var_10], eax
.text:00401009                 push    0FFFFFFF6h      ; nStdHandle
.text:0040100B                 call    GetStdHandle
.text:00401011                 mov     [ebp+var_C], eax
.text:00401014                 push    0FFFFFFF5h      ; nStdHandle
.text:00401016                 call    GetStdHandle
.text:0040101C                 mov     [ebp+hFile], eax
.text:0040101F                 push    0               ; lpOverlapped
.text:00401021                 lea     eax, [ebp+NumberOfBytesWritten]
.text:00401024                 push    eax             ; lpNumberOfBytesWritten
.text:00401025                 push    2Ah             ; nNumberOfBytesToWrite
.text:00401027                 push    offset aLetSStartOutEa ; "Let's start out easy\r\nEnter the passw"...
.text:0040102C                 push    [ebp+hFile]     ; hFile
.text:0040102F                 call    WriteFile
.text:00401035                 push    0               ; lpOverlapped
.text:00401037                 lea     eax, [ebp+NumberOfBytesWritten]
.text:0040103A                 push    eax             ; lpNumberOfBytesRead
.text:0040103B                 push    32h             ; nNumberOfBytesToRead
.text:0040103D                 push    offset byte_402158 ; lpBuffer
.text:00401042                 push    [ebp+var_C]     ; hFile
.text:00401045                 call    ReadFile
.text:0040104B                 xor     ecx, ecx
.text:0040104D
.text:0040104D loc_40104D:                             ; CODE XREF: start+61j
.text:0040104D                 mov     al, byte_402158[ecx]
.text:00401053                 xor     al, 7Dh
.text:00401055                 cmp     al, byte_402140[ecx]
.text:0040105B                 jnz     short loc_40107B
.text:0040105D                 inc     ecx
.text:0040105E                 cmp     ecx, 18h
.text:00401061                 jl      short loc_40104D
.text:00401063                 push    0               ; lpOverlapped
.text:00401065                 lea     eax, [ebp+NumberOfBytesWritten]
.text:00401068                 push    eax             ; lpNumberOfBytesWritten
.text:00401069                 push    12h             ; nNumberOfBytesToWrite
.text:0040106B                 push    offset aYouAreSuccess ; "You are success\r\n"
.text:00401070                 push    [ebp+hFile]     ; hFile
.text:00401073                 call    WriteFile
.text:00401079                 jmp     short loc_401091
.text:0040107B ; ---------------------------------------------------------------------------
.text:0040107B
.text:0040107B loc_40107B:                             ; CODE XREF: start+5Bj
.text:0040107B                 push    0               ; lpOverlapped
.text:0040107D                 lea     eax, [ebp+NumberOfBytesWritten]
.text:00401080                 push    eax             ; lpNumberOfBytesWritten
.text:00401081                 push    12h             ; nNumberOfBytesToWrite
.text:00401083                 push    offset aYouAreFailure ; "You are failure\r\n"
.text:00401088                 push    [ebp+hFile]     ; hFile
.text:0040108B                 call    WriteFile
.text:00401091
.text:00401091 loc_401091:                             ; CODE XREF: start+79j
.text:00401091                 mov     esp, ebp
.text:00401093                 pop     ebp
.text:00401094                 retn
.text:00401094 start           endp
.text:00401094
.text:00401094 ; ---------------------------------------------------------------------------
.text:00401095                 align 200h
.text:00401200                 dd 380h dup(?)
.text:00401200 _text           ends
.text:00401200
.data:00402000 ; Section 2. (virtual address 00002000)
.data:00402000 ; Virtual size                  : 00001000 (   4096.)
.data:00402000 ; Section size in file          : 00000200 (    512.)
.data:00402000 ; Offset to raw data for section: 00000400
.data:00402000 ; Flags C0000040: Data Readable Writable
.data:00402000 ; Alignment     : default
.data:00402000 ; ===========================================================================
.data:00402000
.data:00402000 ; Segment type: Pure data
.data:00402000 ; Segment permissions: Read/Write
.data:00402000 _data           segment para public 'DATA' use32
.data:00402000                 assume cs:_data
.data:00402000                 ;org 402000h
.data:00402000 __IMPORT_DESCRIPTOR_kernel32 dd rva off_402028 ; Import Name Table
.data:00402004                 dd 0                    ; Time stamp
.data:00402008                 dd 0                    ; Forwarder Chain
.data:0040200C                 dd rva aKernel32_dll    ; DLL Name
.data:00402010                 dd rva LoadLibraryA     ; Import Address Table
.data:00402014                 db    0
.data:00402015                 db    0
.data:00402016                 db    0
.data:00402017                 db    0
.data:00402018                 db    0
.data:00402019                 db    0
.data:0040201A                 db    0
.data:0040201B                 db    0
.data:0040201C                 db    0
.data:0040201D                 db    0
.data:0040201E                 db    0
.data:0040201F                 db    0
.data:00402020                 db    0
.data:00402021                 db    0
.data:00402022                 db    0
.data:00402023                 db    0
.data:00402024                 db    0
.data:00402025                 db    0
.data:00402026                 db    0
.data:00402027                 db    0
.data:00402028 ;
.data:00402028 ; Import names for kernel32.dll
.data:00402028 ;
.data:00402028 off_402028      dd rva word_40207D      ; DATA XREF: .data:__IMPORT_DESCRIPTOR_kernel32o
.data:0040202C                 dd rva word_40208C
.data:00402030                 dd rva word_40209D
.data:00402034                 dd rva word_4020AC
.data:00402038                 dd rva word_4020BB
.data:0040203C                 dd rva word_4020CB
.data:00402040                 dd rva word_4020DB
.data:00402044                 dd rva word_4020E7
.data:00402048                 dd 0
.data:00402048 _data           ends
.data:00402048
.idata:0040204C ;
.idata:0040204C ; Imports from kernel32.dll
.idata:0040204C ;
.idata:0040204C ; ===========================================================================
.idata:0040204C
.idata:0040204C ; Segment type: Externs
.idata:0040204C ; _idata
.idata:0040204C ; HMODULE __stdcall LoadLibraryA(LPCSTR lpLibFileName)
.idata:0040204C                 extrn LoadLibraryA:dword ; DATA XREF: .data:00402010o
.idata:00402050 ; FARPROC __stdcall GetProcAddress(HMODULE hModule, LPCSTR lpProcName)
.idata:00402050                 extrn GetProcAddress:dword
.idata:00402054 ; DWORD __stdcall GetLastError()
.idata:00402054                 extrn GetLastError:dword
.idata:00402058 ; HANDLE __stdcall GetStdHandle(DWORD nStdHandle)
.idata:00402058                 extrn GetStdHandle:dword ; CODE XREF: start+Bp
.idata:00402058                                         ; start+16p
.idata:00402058                                         ; DATA XREF: ...
.idata:0040205C ; BOOL __stdcall AttachConsole(DWORD dwProcessId)
.idata:0040205C                 extrn AttachConsole:dword
.idata:00402060 ; BOOL __stdcall WriteConsoleA(HANDLE hConsoleOutput, const void *lpBuffer, DWORD nNumberOfCharsToWrite, LPDWORD lpNumberOfCharsWritten, LPVOID lpReserved)
.idata:00402060                 extrn WriteConsoleA:dword
.idata:00402064 ; BOOL __stdcall WriteFile(HANDLE hFile, LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, LPOVERLAPPED lpOverlapped)
.idata:00402064                 extrn WriteFile:dword   ; CODE XREF: start+2Fp
.idata:00402064                                         ; start+73p ...
.idata:00402068 ; BOOL __stdcall ReadFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, LPDWORD lpNumberOfBytesRead, LPOVERLAPPED lpOverlapped)
.idata:00402068                 extrn ReadFile:dword    ; CODE XREF: start+45p
.idata:00402068                                         ; DATA XREF: start+45r
.idata:00402068
.data:0040206C ; ===========================================================================
.data:0040206C
.data:0040206C ; Segment type: Pure data
.data:0040206C ; Segment permissions: Read/Write
.data:0040206C _data           segment para public 'DATA' use32
.data:0040206C                 assume cs:_data
.data:0040206C                 ;org 40206Ch
.data:0040206C                 align 10h
.data:00402070 aKernel32_dll   db 'kernel32.dll',0     ; DATA XREF: .data:0040200Co
.data:0040207D word_40207D     dw 0                    ; DATA XREF: .data:off_402028o
.data:0040207F                 db 'LoadLibraryA',0
.data:0040208C word_40208C     dw 0                    ; DATA XREF: .data:0040202Co
.data:0040208E                 db 'GetProcAddress',0
.data:0040209D word_40209D     dw 0                    ; DATA XREF: .data:00402030o
.data:0040209F                 db 'GetLastError',0
.data:004020AC word_4020AC     dw 0                    ; DATA XREF: .data:00402034o
.data:004020AE                 db 'GetStdHandle',0
.data:004020BB word_4020BB     dw 0                    ; DATA XREF: .data:00402038o
.data:004020BD                 db 'AttachConsole',0
.data:004020CB word_4020CB     dw 0                    ; DATA XREF: .data:0040203Co
.data:004020CD                 db 'WriteConsoleA',0
.data:004020DB word_4020DB     dw 0                    ; DATA XREF: .data:00402040o
.data:004020DD                 db 'WriteFile',0
.data:004020E7 word_4020E7     dw 0                    ; DATA XREF: .data:00402044o
.data:004020E9                 db 'ReadFile',0
.data:004020F2 aLetSStartOutEa db 'Let',27h,'s start out easy',0Dh,0Ah
.data:004020F2                                         ; DATA XREF: start+27o
.data:004020F2                 db 'Enter the password>',0
.data:0040211C aYouAreSuccess  db 'You are success',0Dh,0Ah,0 ; DATA XREF: start+6Bo
.data:0040212E aYouAreFailure  db 'You are failure',0Dh,0Ah,0 ; DATA XREF: start+83o
.data:00402140 ; char byte_402140[]
.data:00402140 byte_402140     db  1Fh,   8, 13h, 13h,   4, 22h, 0Eh, 11h, 4Dh, 0Dh, 18h
.data:00402140                                         ; DATA XREF: start+55r
.data:00402140                 db  3Dh, 1Bh, 11h, 1Ch, 0Fh, 18h, 50h, 12h, 13h, 53h, 1Eh
.data:00402140                 db  12h, 10h
.data:00402158 ; char byte_402158[]
.data:00402158 byte_402158     db 0                    ; DATA XREF: start+3Do
.data:00402158                                         ; start:loc_40104Dr
.data:00402159                 align 1000h
.data:00402159 _data           ends
.data:00402159
.data:00402159
.data:00402159                 end start