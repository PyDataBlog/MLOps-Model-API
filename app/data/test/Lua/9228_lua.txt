--[[
ReaScript Name: drumbounce
Description: Simulates multiple drum hits from a single MIDI note.
Instructions: Select desired notes and run script.
Screenshot:
Notes: Edit the formula on line 42 to change the dynamics of the bounce.
Category: MIDI Editor
Author: Yak Yak
Author URl: http://notanavocado.github.io/
Repository: GitHub > notanavocado > ReaScripts
Repository URl: https://github.com/notanavocado/Reascripts
File URl: https://github.com/notanavocado/Reascripts/drumbounce.lua
Licence: GPL v2
Forum Thread:
Forum Thread URl:
Version 0.5
REAPER: 5.1
Extensions: None
]]--

retval, retvals_csv = reaper.GetUserInputs("Multiple Bounce Generator", 3, "Iterations:,Falloff (>0-1):,Initial delay:", "5,0.8,150") --gets user parameters
if retval then --did the user click ok?
  counter = 0
  for result in string.gmatch(retvals_csv, '([^,]+)') do
    if counter == 0 then iterations = result --number of new notes added for each selected note
    elseif counter ==1 then falloff = result --the proportion of velocity retained by each subsequent note
    elseif counter ==2 then initdelay = result --the delay between the original note and its first trailing note in MIDI ticks
    end
    counter = counter + 1
  end
  active_take = reaper.MIDIEditor_GetTake(reaper.MIDIEditor_GetActive()) --Gets the current take
  if active_take ~= nil then --required to ensure the selected take is MIDI, avoids errors
    retval, notes, ccs, sysex = reaper.MIDI_CountEvts(active_take) --get stuff from the take
    for i=0, notes-1 do --cycle through all notes in the take
      retval, sel, muted, startppqposOut, endppqposOut, chan, pitch, vel = reaper.MIDI_GetNote(active_take, i) --get data for each note
      if sel==true then --is the note selected?
        newvel = vel --store the velocity of the starting note
        startpos = startppqposOut --store the position of the starting note
        for j=1, iterations+1 do --each cycle will create a new trailing note
          newvel = math.floor(falloff*newvel) --proportionally reduce the velocity to be applied
          startpos = math.floor((1/j * initdelay) + startpos) --increase the starting position
          reaper.MIDI_InsertNote(active_take, false, false, startpos, startpos+20, chan, pitch, newvel) --create a new note
        end
      end
    end
  end
end
reaper.UpdateArrange() --needed to keep things looking shiny
