;   visual - demoscene
;   Copyright (C) 2014  V. S. Lutov
;
;   This program is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.

include 'strucs.asm'

format ELF executable 3

segment executable

entry start
start:

  mov eax,4
  mov ebx,1
  mov ecx,msg
  mov edx,msg.size
  int 0x80

  mov eax,1
  xor ebx,ebx
  int 0x80

segment readable

  msg string 'Hello, World!',0xA

