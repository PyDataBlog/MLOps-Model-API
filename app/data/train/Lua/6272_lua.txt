function getDefaultZoneTrack(zone, isFighting, isInParty, isPartyFighting, isInResidence)
	local returnTrack = 0;
	
	if(zone == 0 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 0 and isFighting) then returnTrack = 0;
	elseif(zone == 0) then returnTrack = 0;
	end;

	if(zone == 1 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 1 and isFighting) then returnTrack = 218;
	elseif(zone == 1) then returnTrack = 229;
	end;

	if(zone == 2 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 2 and isFighting) then returnTrack = 218;
	elseif(zone == 2) then returnTrack = 0;
	end;

	if(zone == 3 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 3 and isFighting) then returnTrack = 218;
	elseif(zone == 3) then returnTrack = 229;
	end;

	if(zone == 4 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 4 and isFighting) then returnTrack = 218;
	elseif(zone == 4) then returnTrack = 0;
	end;

	if(zone == 5 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 5 and isFighting) then returnTrack = 218;
	elseif(zone == 5) then returnTrack = 0;
	end;

	if(zone == 6 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 6 and isFighting) then returnTrack = 220;
	elseif(zone == 6) then returnTrack = 220;
	end;

	if(zone == 7 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 7 and isFighting) then returnTrack = 218;
	elseif(zone == 7) then returnTrack = 0;
	end;

	if(zone == 8 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 8 and isFighting) then returnTrack = 220;
	elseif(zone == 8) then returnTrack = 0;
	end;

	if(zone == 9 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 9 and isFighting) then returnTrack = 218;
	elseif(zone == 9) then returnTrack = 225;
	end;

	if(zone == 10 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 10 and isFighting) then returnTrack = 220;
	elseif(zone == 10) then returnTrack = 0;
	end;

	if(zone == 11 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 11 and isFighting) then returnTrack = 218;
	elseif(zone == 11) then returnTrack = 221;
	end;

	if(zone == 12 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 12 and isFighting) then returnTrack = 218;
	elseif(zone == 12) then returnTrack = 221;
	end;

	if(zone == 13 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 13 and isFighting) then returnTrack = 220;
	elseif(zone == 13) then returnTrack = 0;
	end;

	if(zone == 14 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 14 and isFighting) then returnTrack = 218;
	elseif(zone == 14) then returnTrack = 0;
	end;

	if(zone == 15 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 15 and isFighting) then returnTrack = 52;
	elseif(zone == 15) then returnTrack = 51;
	end;

	if(zone == 16 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 16 and isFighting) then returnTrack = 218;
	elseif(zone == 16) then returnTrack = 222;
	end;

	if(zone == 17 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 17 and isFighting) then returnTrack = 220;
	elseif(zone == 17) then returnTrack = 0;
	end;

	if(zone == 18 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 18 and isFighting) then returnTrack = 218;
	elseif(zone == 18) then returnTrack = 222;
	end;

	if(zone == 19 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 19 and isFighting) then returnTrack = 220;
	elseif(zone == 19) then returnTrack = 0;
	end;

	if(zone == 20 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 20 and isFighting) then returnTrack = 218;
	elseif(zone == 20) then returnTrack = 222;
	end;

	if(zone == 21 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 21 and isFighting) then returnTrack = 220;
	elseif(zone == 21) then returnTrack = 0;
	end;

	if(zone == 22 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 22 and isFighting) then returnTrack = 218;
	elseif(zone == 22) then returnTrack = 222;
	end;

	if(zone == 23 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 23 and isFighting) then returnTrack = 220;
	elseif(zone == 23) then returnTrack = 0;
	end;

	if(zone == 24 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 24 and isFighting) then returnTrack = 218;
	elseif(zone == 24) then returnTrack = 230;
	end;

	if(zone == 25 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 25 and isFighting) then returnTrack = 218;
	elseif(zone == 25) then returnTrack = 230;
	end;

	if(zone == 26 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 26 and isFighting) then returnTrack = 218;
	elseif(zone == 26) then returnTrack = 245;
	end;

	if(zone == 27 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 27 and isFighting) then returnTrack = 218;
	elseif(zone == 27) then returnTrack = 0;
	end;

	if(zone == 28 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 28 and isFighting) then returnTrack = 218;
	elseif(zone == 28) then returnTrack = 0;
	end;

	if(zone == 29 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 29 and isFighting) then returnTrack = 218;
	elseif(zone == 29) then returnTrack = 0;
	end;

	if(zone == 30 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 30 and isFighting) then returnTrack = 218;
	elseif(zone == 30) then returnTrack = 0;
	end;

	if(zone == 31 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 31 and isFighting) then returnTrack = 220;
	elseif(zone == 31) then returnTrack = 0;
	end;

	if(zone == 32 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 32 and isFighting) then returnTrack = 218;
	elseif(zone == 32) then returnTrack = 0;
	end;

	if(zone == 33 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 33 and isFighting) then returnTrack = 218;
	elseif(zone == 33) then returnTrack = 233;
	end;

	if(zone == 34 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 34 and isFighting) then returnTrack = 218;
	elseif(zone == 34) then returnTrack = 228;
	end;

	if(zone == 35 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 35 and isFighting) then returnTrack = 218;
	elseif(zone == 35) then returnTrack = 228;
	end;

	if(zone == 36 and isPartyFighting and isInParty) then returnTrack = 220;
	elseif(zone == 36 and isFighting) then returnTrack = 220;
	elseif(zone == 36) then returnTrack = 0;
	end;

	if(zone == 37 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 37 and isFighting) then returnTrack = 218;
	elseif(zone == 37) then returnTrack = 0;
	end;

	if(zone == 38 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 38 and isFighting) then returnTrack = 218;
	elseif(zone == 38) then returnTrack = 0;
	end;

	if(zone == 39 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 39 and isFighting) then returnTrack = 218;
	elseif(zone == 39) then returnTrack = 121;
	end;

	if(zone == 40 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 40 and isFighting) then returnTrack = 218;
	elseif(zone == 40) then returnTrack = 121;
	end;

	if(zone == 41 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 41 and isFighting) then returnTrack = 218;
	elseif(zone == 41) then returnTrack = 121;
	end;

	if(zone == 42 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 42 and isFighting) then returnTrack = 218;
	elseif(zone == 42) then returnTrack = 121;
	end;

	if(zone == 43 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 43 and isFighting) then returnTrack = 218;
	elseif(zone == 43) then returnTrack = 0;
	end;

	if(zone == 44 and isPartyFighting and isInParty) then returnTrack = 219;
	elseif(zone == 44 and isFighting) then returnTrack = 218;
	elseif(zone == 44) then returnTrack = 0;
	end;

	if(zone == 45 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 45 and isFighting) then returnTrack = 52;
	elseif(zone == 45) then returnTrack = 51;
	end;

	if(zone == 46 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 46 and isFighting) then returnTrack = 139;
	elseif(zone == 46) then returnTrack = 147;
	end;

	if(zone == 47 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 47 and isFighting) then returnTrack = 139;
	elseif(zone == 47) then returnTrack = 147;
	end;

	if(zone == 48 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 48 and isFighting) then returnTrack = 139;
	elseif(zone == 48) then returnTrack = 178;
	end;

	if(zone == 49 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 49 and isFighting) then returnTrack = 0;
	elseif(zone == 49) then returnTrack = 0;
	end;

	if(zone == 50 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 50 and isFighting) then returnTrack = 139;
	elseif(zone == 50) then returnTrack = 178;
	end;

	if(zone == 51 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 51 and isFighting) then returnTrack = 139;
	elseif(zone == 51) then returnTrack = 149;
	end;

	if(zone == 52 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 52 and isFighting) then returnTrack = 139;
	elseif(zone == 52) then returnTrack = 149;
	end;

	if(zone == 53 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 53 and isFighting) then returnTrack = 139;
	elseif(zone == 53) then returnTrack = 175;
	end;

	if(zone == 54 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 54 and isFighting) then returnTrack = 138;
	elseif(zone == 54) then returnTrack = 150;
	end;

	if(zone == 55 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 55 and isFighting) then returnTrack = 138;
	elseif(zone == 55) then returnTrack = 0;
	end;

	if(zone == 56 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 56 and isFighting) then returnTrack = 138;
	elseif(zone == 56) then returnTrack = 0;
	end;

	if(zone == 57 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 57 and isFighting) then returnTrack = 143;
	elseif(zone == 57) then returnTrack = 0;
	end;

	if(zone == 58 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 58 and isFighting) then returnTrack = 139;
	elseif(zone == 58) then returnTrack = 147;
	end;

	if(zone == 59 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 59 and isFighting) then returnTrack = 139;
	elseif(zone == 59) then returnTrack = 147;
	end;

	if(zone == 60 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 60 and isFighting) then returnTrack = 143;
	elseif(zone == 60) then returnTrack = 172;
	end;

	if(zone == 61 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 61 and isFighting) then returnTrack = 139;
	elseif(zone == 61) then returnTrack = 0;
	end;

	if(zone == 62 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 62 and isFighting) then returnTrack = 138;
	elseif(zone == 62) then returnTrack = 0;
	end;

	if(zone == 63 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 63 and isFighting) then returnTrack = 138;
	elseif(zone == 63) then returnTrack = 0;
	end;

	if(zone == 64 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 64 and isFighting) then returnTrack = 143;
	elseif(zone == 64) then returnTrack = 0;
	end;

	if(zone == 65 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 65 and isFighting) then returnTrack = 138;
	elseif(zone == 65) then returnTrack = 0;
	end;

	if(zone == 66 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 66 and isFighting) then returnTrack = 138;
	elseif(zone == 66) then returnTrack = 0;
	end;

	if(zone == 67 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 67 and isFighting) then returnTrack = 143;
	elseif(zone == 67) then returnTrack = 0;
	end;

	if(zone == 68 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 68 and isFighting) then returnTrack = 138;
	elseif(zone == 68) then returnTrack = 174;
	end;

	if(zone == 69 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 69 and isFighting) then returnTrack = 138;
	elseif(zone == 69) then returnTrack = 0;
	end;

	if(zone == 70 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 70 and isFighting) then returnTrack = 139;
	elseif(zone == 70) then returnTrack = 176;
	end;

	if(zone == 71 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 71 and isFighting) then returnTrack = 139;
	elseif(zone == 71) then returnTrack = 0;
	end;

	if(zone == 72 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 72 and isFighting) then returnTrack = 138;
	elseif(zone == 72) then returnTrack = 0;
	end;

	if(zone == 73 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 73 and isFighting) then returnTrack = 138;
	elseif(zone == 73) then returnTrack = 148;
	end;

	if(zone == 74 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 74 and isFighting) then returnTrack = 138;
	elseif(zone == 74) then returnTrack = 148;
	end;

	if(zone == 75 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 75 and isFighting) then returnTrack = 138;
	elseif(zone == 75) then returnTrack = 148;
	end;

	if(zone == 76 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 76 and isFighting) then returnTrack = 138;
	elseif(zone == 76) then returnTrack = 148;
	end;

	if(zone == 77 and isPartyFighting and isInParty) then returnTrack = 138;
	elseif(zone == 77 and isFighting) then returnTrack = 138;
	elseif(zone == 77) then returnTrack = 148;
	end;

	if(zone == 78 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 78 and isFighting) then returnTrack = 143;
	elseif(zone == 78) then returnTrack = 0;
	end;

	if(zone == 79 and isPartyFighting and isInParty) then returnTrack = 139;
	elseif(zone == 79 and isFighting) then returnTrack = 139;
	elseif(zone == 79) then returnTrack = 173;
	end;

	if(zone == 80 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 80 and isFighting) then returnTrack = 215;
	elseif(zone == 80) then returnTrack = 254;
	end;

	if(zone == 81 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 81 and isFighting) then returnTrack = 215;
	elseif(zone == 81) then returnTrack = 251;
	end;

	if(zone == 82 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 82 and isFighting) then returnTrack = 215;
	elseif(zone == 82) then returnTrack = 0;
	end;

	if(zone == 83 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 83 and isFighting) then returnTrack = 215;
	elseif(zone == 83) then returnTrack = 0;
	end;

	if(zone == 84 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 84 and isFighting) then returnTrack = 215;
	elseif(zone == 84) then returnTrack = 252;
	end;

	if(zone == 85 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 85 and isFighting) then returnTrack = 216;
	elseif(zone == 85) then returnTrack = 0;
	end;

	if(zone == 86 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 86 and isFighting) then returnTrack = 216;
	elseif(zone == 86) then returnTrack = 0;
	end;

	if(zone == 87 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 87 and isFighting) then returnTrack = 215;
	elseif(zone == 87) then returnTrack = 180;
	end;

	if(zone == 88 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 88 and isFighting) then returnTrack = 215;
	elseif(zone == 88) then returnTrack = 253;
	end;

	if(zone == 89 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 89 and isFighting) then returnTrack = 215;
	elseif(zone == 89) then returnTrack = 0;
	end;

	if(zone == 90 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 90 and isFighting) then returnTrack = 215;
	elseif(zone == 90) then returnTrack = 0;
	end;

	if(zone == 91 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 91 and isFighting) then returnTrack = 215;
	elseif(zone == 91) then returnTrack = 252;
	end;

	if(zone == 92 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 92 and isFighting) then returnTrack = 216;
	elseif(zone == 92) then returnTrack = 0;
	end;

	if(zone == 93 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 93 and isFighting) then returnTrack = 216;
	elseif(zone == 93) then returnTrack = 0;
	end;

	if(zone == 94 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 94 and isFighting) then returnTrack = 215;
	elseif(zone == 94) then returnTrack = 182;
	end;

	if(zone == 95 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 95 and isFighting) then returnTrack = 215;
	elseif(zone == 95) then returnTrack = 141;
	end;

	if(zone == 96 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 96 and isFighting) then returnTrack = 215;
	elseif(zone == 96) then returnTrack = 0;
	end;

	if(zone == 97 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 97 and isFighting) then returnTrack = 215;
	elseif(zone == 97) then returnTrack = 0;
	end;

	if(zone == 98 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 98 and isFighting) then returnTrack = 215;
	elseif(zone == 98) then returnTrack = 252;
	end;

	if(zone == 99 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 99 and isFighting) then returnTrack = 216;
	elseif(zone == 99) then returnTrack = 0;
	end;

	if(zone == 100 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 100 and isFighting) then returnTrack = 101;
	elseif(zone == 100) then returnTrack = 109;
	end;

	if(zone == 101 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 101 and isFighting) then returnTrack = 101;
	elseif(zone == 101) then returnTrack = 109;
	end;

	if(zone == 102 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 102 and isFighting) then returnTrack = 101;
	elseif(zone == 102) then returnTrack = 0;
	end;

	if(zone == 103 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 103 and isFighting) then returnTrack = 101;
	elseif(zone == 103) then returnTrack = 0;
	end;

	if(zone == 104 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 104 and isFighting) then returnTrack = 101;
	elseif(zone == 104) then returnTrack = 0;
	end;

	if(zone == 105 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 105 and isFighting) then returnTrack = 101;
	elseif(zone == 105) then returnTrack = 114;
	end;

	if(zone == 106 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 106 and isFighting) then returnTrack = 101;
	elseif(zone == 106) then returnTrack = 116;
	end;

	if(zone == 107 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 107 and isFighting) then returnTrack = 101;
	elseif(zone == 107) then returnTrack = 116;
	end;

	if(zone == 108 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 108 and isFighting) then returnTrack = 101;
	elseif(zone == 108) then returnTrack = 0;
	end;

	if(zone == 109 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 109 and isFighting) then returnTrack = 101;
	elseif(zone == 109) then returnTrack = 0;
	end;

	if(zone == 110 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 110 and isFighting) then returnTrack = 101;
	elseif(zone == 110) then returnTrack = 118;
	end;

	if(zone == 111 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 111 and isFighting) then returnTrack = 101;
	elseif(zone == 111) then returnTrack = 0;
	end;

	if(zone == 112 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 112 and isFighting) then returnTrack = 101;
	elseif(zone == 112) then returnTrack = 164;
	end;

	if(zone == 113 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 113 and isFighting) then returnTrack = 191;
	elseif(zone == 113) then returnTrack = 0;
	end;

	if(zone == 114 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 114 and isFighting) then returnTrack = 191;
	elseif(zone == 114) then returnTrack = 171;
	end;

	if(zone == 115 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 115 and isFighting) then returnTrack = 101;
	elseif(zone == 115) then returnTrack = 113;
	end;

	if(zone == 116 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 116 and isFighting) then returnTrack = 101;
	elseif(zone == 116) then returnTrack = 113;
	end;

	if(zone == 117 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 117 and isFighting) then returnTrack = 101;
	elseif(zone == 117) then returnTrack = 0;
	end;

	if(zone == 118 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 118 and isFighting) then returnTrack = 101;
	elseif(zone == 118) then returnTrack = 0;
	end;

	if(zone == 119 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 119 and isFighting) then returnTrack = 101;
	elseif(zone == 119) then returnTrack = 0;
	end;

	if(zone == 120 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 120 and isFighting) then returnTrack = 101;
	elseif(zone == 120) then returnTrack = 158;
	end;

	if(zone == 121 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 121 and isFighting) then returnTrack = 191;
	elseif(zone == 121) then returnTrack = 190;
	end;

	if(zone == 122 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 122 and isFighting) then returnTrack = 191;
	elseif(zone == 122) then returnTrack = 211;
	end;

	if(zone == 123 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 123 and isFighting) then returnTrack = 191;
	elseif(zone == 123) then returnTrack = 134;
	end;

	if(zone == 124 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 124 and isFighting) then returnTrack = 191;
	elseif(zone == 124) then returnTrack = 134;
	end;

	if(zone == 125 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 125 and isFighting) then returnTrack = 191;
	elseif(zone == 125) then returnTrack = 171;
	end;

	if(zone == 126 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 126 and isFighting) then returnTrack = 101;
	elseif(zone == 126) then returnTrack = 0;
	end;

	if(zone == 127 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 127 and isFighting) then returnTrack = 101;
	elseif(zone == 127) then returnTrack = 0;
	end;

	if(zone == 128 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 128 and isFighting) then returnTrack = 191;
	elseif(zone == 128) then returnTrack = 0;
	end;

	if(zone == 129 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 129 and isFighting) then returnTrack = 216;
	elseif(zone == 129) then returnTrack = 0;
	end;

	if(zone == 130 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 130 and isFighting) then returnTrack = 191;
	elseif(zone == 130) then returnTrack = 210;
	end;

	if(zone == 131 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 131 and isFighting) then returnTrack = 101;
	elseif(zone == 131) then returnTrack = 0;
	end;

	if(zone == 132 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 132 and isFighting) then returnTrack = 52;
	elseif(zone == 132) then returnTrack = 51;
	end;

	if(zone == 133 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 133 and isFighting) then returnTrack = 0;
	elseif(zone == 133) then returnTrack = 0;
	end;

	if(zone == 134 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 134 and isFighting) then returnTrack = 115;
	elseif(zone == 134) then returnTrack = 121;
	end;

	if(zone == 135 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 135 and isFighting) then returnTrack = 115;
	elseif(zone == 135) then returnTrack = 119;
	end;

	if(zone == 136 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 136 and isFighting) then returnTrack = 215;
	elseif(zone == 136) then returnTrack = 0;
	end;

	if(zone == 137 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 137 and isFighting) then returnTrack = 215;
	elseif(zone == 137) then returnTrack = 0;
	end;

	if(zone == 138 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 138 and isFighting) then returnTrack = 216;
	elseif(zone == 138) then returnTrack = 0;
	end;

	if(zone == 139 and isPartyFighting and isInParty) then returnTrack = 125;
	elseif(zone == 139 and isFighting) then returnTrack = 125;
	elseif(zone == 139) then returnTrack = 0;
	end;

	if(zone == 140 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 140 and isFighting) then returnTrack = 115;
	elseif(zone == 140) then returnTrack = 0;
	end;

	if(zone == 141 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 141 and isFighting) then returnTrack = 115;
	elseif(zone == 141) then returnTrack = 0;
	end;

	if(zone == 142 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 142 and isFighting) then returnTrack = 115;
	elseif(zone == 142) then returnTrack = 0;
	end;

	if(zone == 143 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 143 and isFighting) then returnTrack = 115;
	elseif(zone == 143) then returnTrack = 0;
	end;

	if(zone == 144 and isPartyFighting and isInParty) then returnTrack = 125;
	elseif(zone == 144 and isFighting) then returnTrack = 125;
	elseif(zone == 144) then returnTrack = 125;
	end;

	if(zone == 145 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 145 and isFighting) then returnTrack = 115;
	elseif(zone == 145) then returnTrack = 0;
	end;

	if(zone == 146 and isPartyFighting and isInParty) then returnTrack = 125;
	elseif(zone == 146 and isFighting) then returnTrack = 125;
	elseif(zone == 146) then returnTrack = 0;
	end;

	if(zone == 147 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 147 and isFighting) then returnTrack = 115;
	elseif(zone == 147) then returnTrack = 0;
	end;

	if(zone == 148 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 148 and isFighting) then returnTrack = 115;
	elseif(zone == 148) then returnTrack = 0;
	end;

	if(zone == 149 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 149 and isFighting) then returnTrack = 115;
	elseif(zone == 149) then returnTrack = 0;
	end;

	if(zone == 150 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 150 and isFighting) then returnTrack = 115;
	elseif(zone == 150) then returnTrack = 0;
	end;

	if(zone == 151 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 151 and isFighting) then returnTrack = 115;
	elseif(zone == 151) then returnTrack = 0;
	end;

	if(zone == 152 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 152 and isFighting) then returnTrack = 115;
	elseif(zone == 152) then returnTrack = 0;
	end;

	if(zone == 153 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 153 and isFighting) then returnTrack = 192;
	elseif(zone == 153) then returnTrack = 0;
	end;

	if(zone == 154 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 154 and isFighting) then returnTrack = 192;
	elseif(zone == 154) then returnTrack = 0;
	end;

	if(zone == 155 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 155 and isFighting) then returnTrack = 216;
	elseif(zone == 155) then returnTrack = 0;
	end;

	if(zone == 156 and isPartyFighting and isInParty) then returnTrack = 217;
	elseif(zone == 156 and isFighting) then returnTrack = 217;
	elseif(zone == 156) then returnTrack = 0;
	end;

	if(zone == 157 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 157 and isFighting) then returnTrack = 115;
	elseif(zone == 157) then returnTrack = 0;
	end;

	if(zone == 158 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 158 and isFighting) then returnTrack = 115;
	elseif(zone == 158) then returnTrack = 0;
	end;

	if(zone == 159 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 159 and isFighting) then returnTrack = 192;
	elseif(zone == 159) then returnTrack = 0;
	end;

	if(zone == 160 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 160 and isFighting) then returnTrack = 192;
	elseif(zone == 160) then returnTrack = 0;
	end;

	if(zone == 161 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 161 and isFighting) then returnTrack = 115;
	elseif(zone == 161) then returnTrack = 155;
	end;

	if(zone == 162 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 162 and isFighting) then returnTrack = 115;
	elseif(zone == 162) then returnTrack = 155;
	end;

	if(zone == 163 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 163 and isFighting) then returnTrack = 193;
	elseif(zone == 163) then returnTrack = 193;
	end;

	if(zone == 164 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 164 and isFighting) then returnTrack = 216;
	elseif(zone == 164) then returnTrack = 0;
	end;

	if(zone == 165 and isPartyFighting and isInParty) then returnTrack = 125;
	elseif(zone == 165 and isFighting) then returnTrack = 125;
	elseif(zone == 165) then returnTrack = 0;
	end;

	if(zone == 166 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 166 and isFighting) then returnTrack = 115;
	elseif(zone == 166) then returnTrack = 0;
	end;

	if(zone == 167 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 167 and isFighting) then returnTrack = 115;
	elseif(zone == 167) then returnTrack = 0;
	end;

	if(zone == 168 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 168 and isFighting) then returnTrack = 193;
	elseif(zone == 168) then returnTrack = 193;
	end;

	if(zone == 169 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 169 and isFighting) then returnTrack = 192;
	elseif(zone == 169) then returnTrack = 0;
	end;

	if(zone == 170 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 170 and isFighting) then returnTrack = 193;
	elseif(zone == 170) then returnTrack = 193;
	end;

	if(zone == 171 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 171 and isFighting) then returnTrack = 216;
	elseif(zone == 171) then returnTrack = 0;
	end;

	if(zone == 172 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 172 and isFighting) then returnTrack = 115;
	elseif(zone == 172) then returnTrack = 0;
	end;

	if(zone == 173 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 173 and isFighting) then returnTrack = 192;
	elseif(zone == 173) then returnTrack = 0;
	end;

	if(zone == 174 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 174 and isFighting) then returnTrack = 192;
	elseif(zone == 174) then returnTrack = 0;
	end;

	if(zone == 175 and isPartyFighting and isInParty) then returnTrack = 216;
	elseif(zone == 175 and isFighting) then returnTrack = 216;
	elseif(zone == 175) then returnTrack = 0;
	end;

	if(zone == 176 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 176 and isFighting) then returnTrack = 192;
	elseif(zone == 176) then returnTrack = 0;
	end;

	if(zone == 177 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 177 and isFighting) then returnTrack = 192;
	elseif(zone == 177) then returnTrack = 207;
	end;

	if(zone == 178 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 178 and isFighting) then returnTrack = 192;
	elseif(zone == 178) then returnTrack = 0;
	end;

	if(zone == 179 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 179 and isFighting) then returnTrack = 193;
	elseif(zone == 179) then returnTrack = 193;
	end;

	if(zone == 180 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 180 and isFighting) then returnTrack = 193;
	elseif(zone == 180) then returnTrack = 0;
	end;

	if(zone == 181 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 181 and isFighting) then returnTrack = 193;
	elseif(zone == 181) then returnTrack = 0;
	end;

	if(zone == 182 and isPartyFighting and isInParty) then returnTrack = 215;
	elseif(zone == 182 and isFighting) then returnTrack = 215;
	elseif(zone == 182) then returnTrack = 0;
	end;

	if(zone == 183 and isPartyFighting and isInParty) then returnTrack = 143;
	elseif(zone == 183 and isFighting) then returnTrack = 143;
	elseif(zone == 183) then returnTrack = 0;
	end;

	if(zone == 184 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 184 and isFighting) then returnTrack = 115;
	elseif(zone == 184) then returnTrack = 0;
	end;

	if(zone == 185 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 185 and isFighting) then returnTrack = 115;
	elseif(zone == 185) then returnTrack = 121;
	end;

	if(zone == 186 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 186 and isFighting) then returnTrack = 115;
	elseif(zone == 186) then returnTrack = 121;
	end;

	if(zone == 187 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 187 and isFighting) then returnTrack = 115;
	elseif(zone == 187) then returnTrack = 121;
	end;

	if(zone == 188 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 188 and isFighting) then returnTrack = 115;
	elseif(zone == 188) then returnTrack = 121;
	end;

	if(zone == 189 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 189 and isFighting) then returnTrack = 0;
	elseif(zone == 189) then returnTrack = 0;
	end;

	if(zone == 190 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 190 and isFighting) then returnTrack = 115;
	elseif(zone == 190) then returnTrack = 0;
	end;

	if(zone == 191 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 191 and isFighting) then returnTrack = 115;
	elseif(zone == 191) then returnTrack = 0;
	end;

	if(zone == 192 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 192 and isFighting) then returnTrack = 115;
	elseif(zone == 192) then returnTrack = 0;
	end;

	if(zone == 193 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 193 and isFighting) then returnTrack = 115;
	elseif(zone == 193) then returnTrack = 0;
	end;

	if(zone == 194 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 194 and isFighting) then returnTrack = 115;
	elseif(zone == 194) then returnTrack = 0;
	end;

	if(zone == 195 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 195 and isFighting) then returnTrack = 115;
	elseif(zone == 195) then returnTrack = 0;
	end;

	if(zone == 196 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 196 and isFighting) then returnTrack = 115;
	elseif(zone == 196) then returnTrack = 0;
	end;

	if(zone == 197 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 197 and isFighting) then returnTrack = 115;
	elseif(zone == 197) then returnTrack = 0;
	end;

	if(zone == 198 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 198 and isFighting) then returnTrack = 115;
	elseif(zone == 198) then returnTrack = 0;
	end;

	if(zone == 199 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 199 and isFighting) then returnTrack = 0;
	elseif(zone == 199) then returnTrack = 0;
	end;

	if(zone == 200 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 200 and isFighting) then returnTrack = 115;
	elseif(zone == 200) then returnTrack = 0;
	end;

	if(zone == 201 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 201 and isFighting) then returnTrack = 193;
	elseif(zone == 201) then returnTrack = 193;
	end;

	if(zone == 202 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 202 and isFighting) then returnTrack = 193;
	elseif(zone == 202) then returnTrack = 193;
	end;

	if(zone == 203 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 203 and isFighting) then returnTrack = 193;
	elseif(zone == 203) then returnTrack = 193;
	end;

	if(zone == 204 and isPartyFighting and isInParty) then returnTrack = 102;
	elseif(zone == 204 and isFighting) then returnTrack = 115;
	elseif(zone == 204) then returnTrack = 0;
	end;

	if(zone == 205 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 205 and isFighting) then returnTrack = 192;
	elseif(zone == 205) then returnTrack = 0;
	end;

	if(zone == 206 and isPartyFighting and isInParty) then returnTrack = 125;
	elseif(zone == 206 and isFighting) then returnTrack = 125;
	elseif(zone == 206) then returnTrack = 125;
	end;

	if(zone == 207 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 207 and isFighting) then returnTrack = 193;
	elseif(zone == 207) then returnTrack = 193;
	end;

	if(zone == 208 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 208 and isFighting) then returnTrack = 192;
	elseif(zone == 208) then returnTrack = 0;
	end;

	if(zone == 209 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 209 and isFighting) then returnTrack = 193;
	elseif(zone == 209) then returnTrack = 193;
	end;

	if(zone == 210 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 210 and isFighting) then returnTrack = 0;
	elseif(zone == 210) then returnTrack = 0;
	end;

	if(zone == 211 and isPartyFighting and isInParty) then returnTrack = 193;
	elseif(zone == 211 and isFighting) then returnTrack = 193;
	elseif(zone == 211) then returnTrack = 193;
	end;

	if(zone == 212 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 212 and isFighting) then returnTrack = 192;
	elseif(zone == 212) then returnTrack = 0;
	end;

	if(zone == 213 and isPartyFighting and isInParty) then returnTrack = 192;
	elseif(zone == 213 and isFighting) then returnTrack = 192;
	elseif(zone == 213) then returnTrack = 0;
	end;

	if(zone == 214 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 214 and isFighting) then returnTrack = 0;
	elseif(zone == 214) then returnTrack = 0;
	end;

	if(zone == 215 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 215 and isFighting) then returnTrack = 52;
	elseif(zone == 215) then returnTrack = 51;
	end;

	if(zone == 216 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 216 and isFighting) then returnTrack = 52;
	elseif(zone == 216) then returnTrack = 51;
	end;

	if(zone == 217 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 217 and isFighting) then returnTrack = 52;
	elseif(zone == 217) then returnTrack = 51;
	end;

	if(zone == 218 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 218 and isFighting) then returnTrack = 52;
	elseif(zone == 218) then returnTrack = 51;
	end;

	if(zone == 219 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 219 and isFighting) then returnTrack = 0;
	elseif(zone == 219) then returnTrack = 0;
	end;

	if(zone == 220 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 220 and isFighting) then returnTrack = 101;
	elseif(zone == 220) then returnTrack = 106;
	end;

	if(zone == 221 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 221 and isFighting) then returnTrack = 101;
	elseif(zone == 221) then returnTrack = 106;
	end;

	if(zone == 222 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 222 and isFighting) then returnTrack = 0;
	elseif(zone == 222) then returnTrack = 0;
	end;

	if(zone == 223 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 223 and isFighting) then returnTrack = 101;
	elseif(zone == 223) then returnTrack = 128;
	end;

	if(zone == 224 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 224 and isFighting) then returnTrack = 101;
	elseif(zone == 224) then returnTrack = 128;
	end;

	if(zone == 225 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 225 and isFighting) then returnTrack = 101;
	elseif(zone == 225) then returnTrack = 128;
	end;

	if(zone == 226 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 226 and isFighting) then returnTrack = 191;
	elseif(zone == 226) then returnTrack = 128;
	end;

	if(zone == 227 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 227 and isFighting) then returnTrack = 101;
	elseif(zone == 227) then returnTrack = 106;
	end;

	if(zone == 228 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 228 and isFighting) then returnTrack = 101;
	elseif(zone == 228) then returnTrack = 106;
	end;

	if(zone == 229 and isPartyFighting and isInParty) then returnTrack = 0;
	elseif(zone == 229 and isFighting) then returnTrack = 0;
	elseif(zone == 229) then returnTrack = 0;
	end;

	if(zone == 230 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 230 and isFighting) then returnTrack = 101;
	elseif(zone == 230) then returnTrack = 107;
	end;

	if(zone == 231 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 231 and isFighting) then returnTrack = 101;
	elseif(zone == 231) then returnTrack = 107;
	end;

	if(zone == 232 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 232 and isFighting) then returnTrack = 101;
	elseif(zone == 232) then returnTrack = 107;
	end;

	if(zone == 233 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 233 and isFighting) then returnTrack = 101;
	elseif(zone == 233) then returnTrack = 156;
	end;

	if(zone == 234 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 234 and isFighting) then returnTrack = 101;
	elseif(zone == 234) then returnTrack = 152;
	end;

	if(zone == 235 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 235 and isFighting) then returnTrack = 101;
	elseif(zone == 235) then returnTrack = 152;
	end;

	if(zone == 236 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 236 and isFighting) then returnTrack = 101;
	elseif(zone == 236) then returnTrack = 152;
	end;

	if(zone == 237 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 237 and isFighting) then returnTrack = 101;
	elseif(zone == 237) then returnTrack = 154;
	end;

	if(zone == 238 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 238 and isFighting) then returnTrack = 101;
	elseif(zone == 238) then returnTrack = 151;
	end;

	if(zone == 239 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 239 and isFighting) then returnTrack = 101;
	elseif(zone == 239) then returnTrack = 151;
	end;

	if(zone == 240 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 240 and isFighting) then returnTrack = 101;
	elseif(zone == 240) then returnTrack = 151;
	end;

	if(zone == 241 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 241 and isFighting) then returnTrack = 101;
	elseif(zone == 241) then returnTrack = 151;
	end;

	if(zone == 242 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 242 and isFighting) then returnTrack = 101;
	elseif(zone == 242) then returnTrack = 162;
	end;

	if(zone == 243 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 243 and isFighting) then returnTrack = 101;
	elseif(zone == 243) then returnTrack = 117;
	end;

	if(zone == 244 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 244 and isFighting) then returnTrack = 101;
	elseif(zone == 244) then returnTrack = 110;
	end;

	if(zone == 245 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 245 and isFighting) then returnTrack = 101;
	elseif(zone == 245) then returnTrack = 110;
	end;

	if(zone == 246 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 246 and isFighting) then returnTrack = 101;
	elseif(zone == 246) then returnTrack = 110;
	end;

	if(zone == 247 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 247 and isFighting) then returnTrack = 191;
	elseif(zone == 247) then returnTrack = 208;
	end;

	if(zone == 248 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 248 and isFighting) then returnTrack = 101;
	elseif(zone == 248) then returnTrack = 112;
	end;

	if(zone == 249 and isPartyFighting and isInParty) then returnTrack = 103;
	elseif(zone == 249 and isFighting) then returnTrack = 101;
	elseif(zone == 249) then returnTrack = 105;
	end;

	if(zone == 250 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 250 and isFighting) then returnTrack = 191;
	elseif(zone == 250) then returnTrack = 135;
	end;

	if(zone == 251 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 251 and isFighting) then returnTrack = 191;
	elseif(zone == 251) then returnTrack = 213;
	end;

	if(zone == 252 and isPartyFighting and isInParty) then returnTrack = 191;
	elseif(zone == 252 and isFighting) then returnTrack = 191;
	elseif(zone == 252) then returnTrack = 209;
	end;

	if(zone == 253 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 253 and isFighting) then returnTrack = 52;
	elseif(zone == 253) then returnTrack = 51;
	end;

	if(zone == 254 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 254 and isFighting) then returnTrack = 52;
	elseif(zone == 254) then returnTrack = 51;
	end;

	if(zone == 255 and isPartyFighting and isInParty) then returnTrack = 52;
	elseif(zone == 255 and isFighting) then returnTrack = 52;
	elseif(zone == 255) then returnTrack = 51;
	end;

	if(isInResidence) then
		returnTrack = 126;
	end
	
	return returnTrack;
end;