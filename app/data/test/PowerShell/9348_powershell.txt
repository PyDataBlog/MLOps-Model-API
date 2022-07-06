#3CX full backup script All paths assume default install location.  $arg3 is the destination of the backup.
#tested on 3cx 11 and 12.5
# other switches that could be used
#/callrecordings        backup extension call recordings
#/voicemails            backup stored voicemails
#/voiceprompts          backup voice prompts and music on hold
#/callhist              backup call history tables from database
#/exit                  close window after finishing backup or restore (for not hidden mode only)
#/firmware              backup stored firmwares
#The follow switches show up in 3cx 12.5.  They do not work in v11
#/ssl                   backup HTTPS (SSL) configuration
#/skipssl               do not restore HTTP (SSL) configuration from backup
$date = get-date -uformat %m%d%Y
$cmd = 'c:\Program Files\3CX PhoneSystem\Bin\3CXBackup.exe'
$arg1 = 'hidden'
$arg2 = 'backup'
$arg3 = "z:\$date.zip"
$arg4 = '/callrecordings'
$arg5 = '/voicemails'
$arg6 = '/voiceprompts'
$arg7 = '/callhist'
$arg8 = '/exit'
& $CMD $arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7 $arg8
