Get-CalendarProcessing -Identity “housr” | format-list

Set-CalendarProcessing -identity “phxsr” -DeleteSubject $false -DeleteComments $false -AddOrganizerToSubject $false  
