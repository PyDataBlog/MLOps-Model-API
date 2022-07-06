forfiles /P "C:\HOW2SAY\logs" /D -30 /C "cmd /c DEL /Q @FILE"
forfiles /P "C:\HOW2SAY\SAVE" /D -2 /C "cmd /c DEL /Q @FILE"
