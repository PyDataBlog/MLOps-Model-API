# Change to SteamCMD install directory
cd c:\steamcmd

# Update / Install the Server
./steamcmd +runscript scripts\csgo.txt

# Run the server with defaults
cd servers/csgo
./srcds -console -fork -usercon -game csgo +map de_dust2 +sv_lan 1 +rcon_password qj123