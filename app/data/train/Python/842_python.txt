#!/usr/bin/python3
from colorama import Fore, Back


class frets:
    tuning = list()
    max_string_name_len = 0;
    frets_count = 0;
    strings = dict()
    NOTES = ('E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B', 'C', 'C#', 'D', 'D#')
    

    def __init__(self,
                 tuning=('E', 'A', 'D', 'G'), 
                 frets_count=24):
        self.tuning = tuning
        self.frets_count = frets_count
        
        for string in tuning:
            if len(string) > self.max_string_name_len:
                self.max_string_name_len = len(string)
        
            padding_count = 0;
            padding = ''
            
            self.strings[string] = list()
            
            starting_note = self.NOTES.index(string) + 1
            
            for i in range(frets_count):
                padding = '^' * int(((starting_note + i) / len(self.NOTES)))
                self.strings[string].append(self.NOTES[(starting_note + i) % len(self.NOTES)] + padding)
                #print('{}{} ({}) = {}'.format(string, 
                #                              i,
                #                              int(((starting_note + i) / len(self.NOTES))),
                #                              self.NOTES[(starting_note + i) % len(self.NOTES)] + padding))
    
    def debug_strings(self):
        print(self.strings)
    
    def show_me_plz(self,
                    seek_note=None,
                    seek_string=None):
        if (seek_string):
            seek_note = self.strings[seek_string[0]][int(seek_string[1]) - 1]
            
        upper_seek_note = None
        lower_seek_note = None
        if seek_note and seek_note.endswith('^'):
            lower_seek_note = seek_note[0:-1]
        if seek_note:
            upper_seek_note = seek_note + '^'
        
        upper_found_position = list()
        found_position = list()
        lower_found_position = list()
                    
        print(Fore.WHITE + \
              ' ' * (self.max_string_name_len + 2), 
              end='')
    
        for fret_nr in range(1, self.frets_count + 1):
            print(Fore.WHITE + \
                  (' ' * (4 - len(str(fret_nr)))) + str(fret_nr), 
                  end='')
            print(Fore.YELLOW + '|', end='')
        print('')
    
        for string in reversed(self.tuning):     
            color = Fore.WHITE + Back.BLACK       
            if string == seek_note:
                color = Fore.WHITE + Back.RED
                found_position.append(string + "0")
            elif string == upper_seek_note:
                color = Fore.WHITE + Back.CYAN
                upper_found_position.append(string + "0")
            elif string == lower_seek_note:
                color = Fore.WHITE + Back.MAGENTA
                lower_found_position.append(string + "0")
            
            print(color + \
                  (' ' * (self.max_string_name_len - len(string))) + \
                  string, end='')
            
            print(Fore.YELLOW + '||', end='')
            
            fret_nr = 1
            
            for note in self.strings[string]:
                color = Fore.WHITE + Back.BLACK
                if note == seek_note:
                    color = Fore.WHITE + Back.RED
                    found_position.append(string + str(fret_nr))
                elif note == upper_seek_note:
                    color = Fore.WHITE + Back.CYAN
                    upper_found_position.append(string + str(fret_nr))
                elif note == lower_seek_note:
                    color = Fore.WHITE + Back.MAGENTA
                    lower_found_position.append(string + str(fret_nr))
                          
                print(color + \
                      note[0:4] + \
                      '-' * (4 - len(note)), end='')
                print(Fore.YELLOW + Back.BLACK + '|', end='')
                
                fret_nr += 1
                
            print(Fore.WHITE + Back.BLACK + '')

        print(Fore.WHITE + '\n')
        
        print(Back.CYAN + ' ' + Back.BLACK + \
              ' Found octave-higher note {} on: {}'.format(upper_seek_note, 
                                                           upper_found_position))
        print(Back.RED + ' ' + Back.BLACK + \
              ' Found note {} on: {}'.format(seek_note, 
                                             found_position))
        print(Fore.WHITE + \
              Back.MAGENTA + ' ' + Back.BLACK + \
              ' Found octave-lower note {} on: {}'.format(lower_seek_note, 
                                                          lower_found_position))
                
            
        
