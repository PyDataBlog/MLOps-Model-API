:: Use as "call world1 [Room###] [Extra info]"
:: use as "call world1 000 [Extra info]"
::set roomInfo=%2
:: echo %2
:: echo %roomInfo%
echo(
if "%2"=="die" set charPos=155
if "%2"=="talk" echo *** Please use who, talk is not a command. Type help for more.
if "%2"=="map" goto :map




goto :Room%1
goto :eof

:map
set /a mapXY=%charPos% - 100
set /a mapY=%mapXY%
set /a mapX=%mapXY%

:whileMapY
if %mapY% GTR 10 (
    set /a mapY=%mapY% - 10
    goto :whileMapY
)

:whileMapX
if %mapX%==10 goto :whileMapXexit
if %mapX%==20 goto :whileMapXexit
if %mapX%==30 goto :whileMapXexit
if %mapX%==40 goto :whileMapXexit
if %mapX%==50 goto :whileMapXexit
if %mapX%==60 goto :whileMapXexit
if %mapX%==70 goto :whileMapXexit
if %mapX%==80 goto :whileMapXexit
if %mapX%==90 goto :whileMapXexit
set /a mapX=%mapX% - 1
goto :whileMapX

:whileMapXexit
set /a mapX=%mapX% / 10

if %mapXY%==11 (set map11=@) else (set map11=#)
if %mapXY%==21 (set map21=@) else (set map21=#)
if %mapXY%==41 (set map41=@) else (set map41=#)
if %mapXY%==51 (set map51=@) else (set map51=#)
if %mapXY%==61 (set map61=@) else (set map61=*)

if %mapXY%==22 (set map22=@) else (set map22=#)
if %mapXY%==32 (set map32=@) else (set map32=#)
if %mapXY%==42 (set map42=@) else (set map42=#)
if %mapXY%==52 (set map52=@) else (set map52=#)
if %mapXY%==62 (set map62=@) else (set map62=#)

if %mapXY%==13 (set map13=@) else (set map13=#)
if %mapXY%==23 (set map23=@) else (set map23=#)
if %mapXY%==33 (set map33=@) else (set map33=#)
if %mapXY%==43 (set map43=@) else (set map43=#)
if %mapXY%==63 (set map63=@) else (set map63=#)
if %mapXY%==73 (set map73=@) else (set map73=#)

if %mapXY%==24 (set map24=@) else (set map24=#)
if %mapXY%==34 (set map34=@) else (set map34=#)
if %mapXY%==44 (set map44=@) else (set map44=#)
if %mapXY%==64 (set map64=@) else (set map64=#)

if %mapXY%==25 (set map25=@) else (set map25=#)
if %mapXY%==35 (set map35=@) else (set map35=#)
if %mapXY%==45 (set map45=@) else (set map45=#)
if %mapXY%==55 (set map55=@) else (set map55=$)
if %mapXY%==65 (set map65=@) else (set map65=#)
if %mapXY%==75 (set map75=@) else (set map75=#)
if %mapXY%==85 (set map85=@) else (set map85=#)
if %mapXY%==95 (set map95=@) else (set map95=*)

if %mapXY%==16 (set map16=@) else (set map16=#)
if %mapXY%==26 (set map26=@) else (set map26=#)
if %mapXY%==36 (set map36=@) else (set map36=#)
if %mapXY%==56 (set map56=@) else (set map56=#)
if %mapXY%==76 (set map76=@) else (set map76=#)

if %mapXY%==17 (set map17=@) else (set map17=#)
if %mapXY%==27 (set map27=@) else (set map27=#)
if %mapXY%==37 (set map37=@) else (set map37=#)
if %mapXY%==57 (set map57=@) else (set map57=#)
if %mapXY%==77 (set map77=@) else (set map77=#)

if %mapXY%==38 (set map38=@) else (set map38=#)
if %mapXY%==48 (set map48=@) else (set map48=#)
if %mapXY%==58 (set map58=@) else (set map58=#)
if %mapXY%==68 (set map68=@) else (set map68=#)
if %mapXY%==78 (set map78=@) else (set map78=#)
if %mapXY%==88 (set map88=@) else (set map88=#)

if %mapXY%==39 (set map39=@) else (set map39=#)
if %mapXY%==49 (set map49=@) else (set map49=#)
if %mapXY%==79 (set map79=@) else (set map79=#)
if %mapXY%==89 (set map89=@) else (set map89=#)

::echo(
:: if %mapXY%==XY (set mapXY=@) else (set mapXY=#)
::echo %mapXY%
echo You are here: @ %mapX%, %mapY%
echo(                 
echo +  1 2 3 4 5 6 7 8 9 X
echo 1- %map11%-%map21%   %map41%-%map51%-%map61%         
echo      ^|   ^| ^|       
echo 2-   %map22%-%map32%-%map42%-%map52%-%map62%
echo      ^| ^| ^|   ^|
echo 3- %map13%-%map23%-%map33% %map43%   %map63%-%map73%
echo      ^|   ^|   ^|              
echo 4-   %map24%-%map34%-%map44%   %map64%          
echo      ^|   ^|   ^|        
echo 5-   %map25%-%map35%-%map45%-%map55%-%map65%-%map75%-%map85%-%map95%
echo      ^|     ^|   ^|
echo 6- %map16%-%map26%-%map36%   %map56%   %map76%
echo    ^|   ^|   ^|   ^|
echo 7- %map17%-%map27%-%map37%   %map57%   %map77%
echo        ^|   ^|
echo 8-     %map38%-%map48%-%map58%-%map68%-%map78%-%map88%
echo          ^|     ^| ^|
echo 9-     %map39%-%map49%     %map79% %map89%  
echo Y
goto :eof
 
:: ##########################################

       1 2 3 4 5 6 7 8 9 
echo 1 #-#   #-#-*         
echo     |   | |       
echo 2   #-#-#-#-#
echo     | | |   |
echo 3 #-#-# #   #-#
echo     |   |   |              
echo 4   #-#-#   #          
echo     |   |   |        
echo 5   #-#-#-$-#-#-#-*
echo     |     |   |
echo 6 #-#-#   #   #
echo   |   |   |   |
echo 7 #-#-#   #   #
echo       |   |
echo 8     #-#-#-#-#-#
echo         |     | |
echo 9     #-#     # #  

::##########################################
::##########################################
:: Rooms - $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

:Room111
set cgn=0
set cge=1
set cgs=0
set cgw=0
::E121
goto :eof

:Room121
set cgn=0
set cge=0
set cgs=1
set cgw=1
::S122
::W121
goto :eof

:Room141
set cgn=0
set cge=1
set cgs=1
set cgw=0
::E151
::S142
goto :eof

:Room151
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E161
::S152
::W141
goto :eof

:Room161 *
set cgn=0
set cge=0
set cgs=0
set cgw=1
::W151
::Take to World 2
goto :eof


::---------------------------------------------
:Room122
set cgn=1
set cge=1
set cgs=1
set cgw=0
::N121
::E132
::S123
goto :eof

:Room132
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E142
::S133
::W122
goto :eof

:Room142
set cgn=1
set cge=1
set cgs=1
set cgw=1
::N141
::E152
::S143
::W132
goto :eof

:Room152
set cgn=1
set cge=1
set cgs=0
set cgw=1
::N151
::E162
::W142
goto :eof

:Room162
set cgn=0
set cge=0
set cgs=1
set cgw=1
::S163
::W152
goto :eof


::---------------------------------------------
:Room113
set cgn=0
set cge=1
set cgs=0
set cgw=0
::E123
goto :eof

:Room123
set cgn=1
set cge=1
set cgs=1
set cgw=1
::N122
::E133
::S124
::W113
goto :eof

:Room133
set cgn=1
set cge=0
set cgs=0
set cgw=1
::N132
::E143
::W123
goto :eof

:Room143
set cgn=1
set cge=0
set cgs=1
set cgw=0
::N142
::S144
goto :eof

:Room163
set cgn=1
set cge=1
set cgs=1
set cgw=0
::N162
::E173
::S164
goto :eof

:Room173
set cgn=0
set cge=0
set cgs=0
set cgw=1
::W163
goto :eof


::---------------------------------------------
:Room124
set cgn=1
set cge=1
set cgs=1
set cgw=0
::N123
::E134
::S125
goto :eof

:Room134
set cgn=0
set cge=1
set cgs=0
set cgw=1
::E144
::W124
goto :eof

:Room144
set cgn=1
set cge=0
set cgs=1
set cgw=1
::N143
::S145
::W134
goto :eof

:Room164
set cgn=1
set cge=0
set cgs=1
set cgw=0
::N163
::S165
goto :eof


::---------------------------------------------
:Room125
set cgn=1
set cge=1
set cgs=1
set cgw=0
::N124
::E135
::S126
goto :eof

:Room135
set cgn=0
set cge=1
set cgs=0
set cgw=1
::E145
::W125
goto :eof

:Room145
set cgn=1
set cge=1
set cgs=0
set cgw=1
::N144
::E155
::W135
goto :eof

:Room155 $
echo ---You are in the center of town. No one seems to be around.
if /I "%2"=="david" echo +++David - I don't want to talk!
if /I "%2"=="anna" echo +++David - How is anna? I'm the only one here. You ok buddy?
if /I "%2"=="who" echo ***David, Anna are here.
if /I "%2"=="shop" goto :shop
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E165
::S156
::W145
goto :eof

:Room165
set cgn=1
set cge=1
set cgs=0
set cgw=1
::N164
::E175
::W155
goto :eof

:Room175
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E185
::S176
::W165
goto :eof

:Room185
set cgn=0
set cge=1
set cgs=0
set cgw=1
::E195
::W175
goto :eof

:Room195 *
set cgn=0
set cge=0
set cgs=0
set cgw=1
::W185
::To world 3
goto :eof


::---------------------------------------------
:Room116
set cgn=0
set cge=1
set cgs=1
set cgw=0
::E126
::S117
goto :eof

:Room126
set cgn=1
set cge=1
set cgs=0
set cgw=1
::N125
::E136
::W116
goto :eof

:Room136
set cgn=0
set cge=0
set cgs=1
set cgw=1
::S137
::W126
goto :eof

:Room156
set cgn=1
set cge=0
set cgs=1
set cgw=0
::N155
::S157
goto :eof

:Room176
set cgn=1
set cge=0
set cgs=1
set cgw=0
::N175
::S177
goto :eof


::---------------------------------------------
:Room117
set cgn=1
set cge=1
set cgs=0
set cgw=0
::N116
::E127
goto :eof

:Room127
set cgn=0
set cge=1
set cgs=0
set cgw=1
::E137
::W117
goto :eof

:Room137
set cgn=1
set cge=0
set cgs=1
set cgw=1
::N137
::S138
::W127
goto :eof

:Room157
set cgn=1
set cge=0
set cgs=1
set cgw=0
::N156
::S158
goto :eof

:Room177
set cgn=1
set cge=0
set cgs=0
set cgw=0
::N176
goto :eof


::---------------------------------------------
:Room138
set cgn=1
set cge=1
set cgs=0
set cgw=0
::N137
::E148
goto :eof

:Room148
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E158
::W138
::S149
goto :eof

:Room158
set cgn=1
set cge=1
set cgs=0
set cgw=1
::N157
::E168
::W148
goto :eof

:Room168
set cgn=0
set cge=1
set cgs=0
set cgw=1
::E178
::W158
goto :eof

:Room178
set cgn=0
set cge=1
set cgs=1
set cgw=1
::E188
::S179
::W168
goto :eof

:Room188
set cgn=0
set cge=0
set cgs=1
set cgw=1
::S189
::W178
goto :eof

::---------------------------------------------
:Room139
set cgn=0
set cge=1
set cgs=0
set cgw=0
::E149
goto :eof

:Room149
set cgn=1
set cge=0
set cgs=0
set cgw=1
::N148
::W139
goto :eof

:Room179
set cgn=1
set cge=0
set cgs=0
set cgw=0
::N178
goto :eof

:Room189
set cgn=1
set cge=0
set cgs=0
set cgw=0
::N188
goto :eof


:shop
set roomInfo=shopX
echo Went to shop
pause
goto :eof
