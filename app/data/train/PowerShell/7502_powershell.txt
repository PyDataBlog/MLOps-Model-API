# PowerShell ISE profile
# See http://technet.microsoft.com/en-us/library/dd819500.aspx for the ISE Object Model heirarchy

# Scripts for the Solarized theme as well as to set file encoding
C:\Users\rakhesh\Code\Solarize-PSISE\Solarize-PSISE-AddOnMenu.ps1 -Apply
C:\Users\rakhesh\Code\Set-PSISEEncoding\Set-PSISEEncoding.ps1

# Useful Add-ons menu entries
# This one switches the Console pane/ Command pane directory to be that of the script in the Script pane
# Piping the output to Out-Null so it doesn't litter the Command pane/ Output pane
$psISE.CurrentPowerShellTab.AddOnsMenu.Submenus.Add("Go to script folder",{cd (split-path $psISE.CurrentFile.FullPath)},"Alt+Shift+C") | Out-Null

# Adds a menu item Add-ons > Open PowerShell ISE profile
# Idea for $null from http://technet.microsoft.com/en-us/library/dd819494.aspx
$psISE.CurrentPowerShellTab.AddOnsMenu.SubMenus.Add("Open PowerShell ISE profile",{$psIse.CurrentPowerShellTab.Files.Add($profile)},$null) | Out-Null

# Set the Output Pane background color to Noble Blue and foreground color to White
# Thanks to http://www.leeholmes.com/blog/2008/06/01/powershells-noble-blue/ for the color code & name
# Commented out as I use the Solarized colors now. 
#$psISE.Options.OutputPaneForegroundColor = "white"
#$psISE.Options.OutputPaneBackgroundColor = $psISE.Options.OutputPaneTextBackgroundColor = "#012456"

# Put Command Pane above (between Script & Output panes) (Only applies to PowerShell 2.0)
if ($PSVersionTable.PSVersion.Major -eq 2) { $psISE.Options.CommandPaneUp = $true }

# Ideas for later:
# 1) http://stackoverflow.com/questions/13500112/how-can-i-map-ctrlshifts-to-save-all-files-in-powershell-ise
# 2) http://blogs.technet.com/b/jamesone/archive/2009/09/02/how-to-load-files-into-the-powershell-ise-editor-or-what-s-in-your-profile.aspx