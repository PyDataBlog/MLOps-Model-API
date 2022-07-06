*************************************************************************** %
%
%  Copyright (c) 2003-2019 Clifford Thompson
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% *************************************************************************** %

% ******************************** %
%                                  %
%      Class: MCPS 791             %
%      Prof : Dr.Charles Brown     %
%      File : lexer.pl             %
%                                  %
% ******************************** %

% ****************************************************************** %
%                                                                    %
% This file contain predicates that collectively process a corpus    %
% Individual words are recognized and stored in an output file. In   %
% this file there is only one word per line                          %
%                                                                    %
% ****************************************************************** %

/************************************************************************/
/*  PREDICATE: open_file/3					        */
/*  CALLED-BY: read_corp/4       				        */
/*									*/
/*  PREDICATE: readInFile/2		            	        	*/
/*  CALLED-BY: read_corp/4					        */
/*									*/
/*  PREDICATE: writeOutFile/2				        	*/
/*  CALLED-BY: read_corp/4					        */
/*									*/
/*  PREDICATE: read_corp/4       			        	*/
/*  CALLED-BY: <user>                   			        */
/*									*/
/*  PREDICATE: read_words/2				        	*/
/*  CALLED-BY: read_corp/4					        */
/*									*/
/*  PREDICATE: next_word/3				        	*/
/*  CALLED-BY: read_words/2					        */
/*									*/
/*  PREDICATE: remove_bad/2				        	*/
/*  CALLED-BY: next_word/3					        */
/*									*/
/************************************************************************/


%
% module libraries
%


:- use_module(library(lists)).  /* used for sorting and removal of duplicates from */
                                /* the list of words in the corpus                 */
%
% open_file(+FileName,-OpenFile,+Type)
%
% opens a file with the name "FileName" with the option of reading and
% writing stored in the term "Type"
%

open_file(FileName,OpenFile,Type) :- open(FileName,Type,OpenFile).


%
% readInFile(+OpenFile,-List)
%
% reads all the characters in the "OpenFile" stream into the
% list "List". Note that the EOF character is not put into this
% list. The file is closed after all characters have been read.


readInFile(OpenFile,[])       :- peek_char(OpenFile,N) , N = -1, close(OpenFile),!.
readInFile(OpenFile,[X|List]) :- get_code(OpenFile,X),readInFile(OpenFile,List).

%
% writeOutFile(+OpenFile,+List)
%
% writes all the characters in the list, "List", into the
% stream "OpenFile". The file is closed after all characters have been
% written.

writeOutFile(OpenFile,[]) :- close(OpenFile),!.

/* tagged as nouns */

%writeOutFile(OpenFile,[Word|List]) :-

%	atom_chars(Word,[Letter|_Ls]),
%	Letter > 64,
%	Letter < 91,
%	write(OpenFile,word( (noun([cat:noun]) --> [Word]))),
%	write(OpenFile,'.\n'),
%	writeOutFile(OpenFile,List).

/* tagged as numbers */

writeOutFile(OpenFile,[Word|List]) :-

	atom_chars(Word,[Letter|_Ls]),
	Letter > 47,
	Letter < 58,
	writeOutFile(OpenFile,List).

/* tagged as unknown word types */

writeOutFile(OpenFile,[Word|List]) :-

	nl, write('Word = ['),write(Word), write('] --- '),
	valid_cats(ValidCats), write(ValidCats), write(': '),
	read(Letter),
	get_features(Letter,FeatureList), match_letter(Letter,Cat),
	write(OpenFile,Cat), write(OpenFile,'('), write(OpenFile,FeatureList),
        write(OpenFile,') --> ['), write(OpenFile,Word), write(OpenFile,']'),
	write(OpenFile,'.\n'),
	writeOutFile(OpenFile,List).







%
% read_corp(+InFile,+Outfile,-NoDupWords,-Words)
%
% this is the main predicate. It opens the file specified in the
% file "InFile", reads the words into a list and outputs one word
% per line into the file "OutFile".
%

read_corp(InFile,OutFile,NoDupWords,Words) :-

	open_file(InFile,OpenFile,read),
	readInFile(OpenFile,Corpus),
	read_words(Corpus,Words),
	sort(Words,SortedWords),
	remove_duplicates(SortedWords,NoDupWords),
        open_file(OutFile,OpenFile2,write),
	writeOutFile(OpenFile2,NoDupWords).

%
% read_words(+Corpus,-WordList)
%
% This predicate reads in the letters stored in "Corpus" and
% creates a list of words in "WordList"
%

read_words([],[]).

read_words(Corpus,[WordAtom|Ws]) :-

	next_word(Corpus,NewCorpus,WordCharList),
	atom_chars(WordAtom,WordCharList),
	read_words(NewCorpus,Ws).

%
% next_word(+Corpus,-NewCorpus,-WorCharList)
%
% This predicate reads the next word in the character corpus
% stored in "Corpus". "NewCorpus" is the list of remaining
% letters after reading in a word. A space signifies a new
% word and certain characters are ignored. The list of ignored
% words is commented below.
%

next_word([],[],[]).

/* Start of ignored characters */

next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 10, remove_bad(Corpus,NewCorpus). /* LF    */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 32, remove_bad(Corpus,NewCorpus). /* SPACE */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 33, remove_bad(Corpus,NewCorpus). /* !     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 34, remove_bad(Corpus,NewCorpus). /* "     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 40, remove_bad(Corpus,NewCorpus). /* (     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 41, remove_bad(Corpus,NewCorpus). /* )     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 44, remove_bad(Corpus,NewCorpus). /* ,     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 45, remove_bad(Corpus,NewCorpus). /* -     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 46, remove_bad(Corpus,NewCorpus). /* .     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 58, remove_bad(Corpus,NewCorpus). /* ?     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 59, remove_bad(Corpus,NewCorpus). /* :     */
next_word([Letter|Corpus],NewCorpus,[]) :- Letter == 63, remove_bad(Corpus,NewCorpus). /* ;     */

/* End of ignored characters */

next_word([Letter|Corpus],NewCorpus,[Letter|Ls]) :-

	next_word(Corpus,NewCorpus,Ls).


%
% remove_bad(+Corpus,-NewCorpus)
%
% This predicate facilitates the ignoring of undesired characters. These
% characters are the same characters as in next_word predicate
%

remove_bad([],[]).

remove_bad([Letter|Corpus],NewCorpus) :- Letter == 9, remove_bad(Corpus,NewCorpus). /* HT    */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 10,remove_bad(Corpus,NewCorpus). /* LF    */

remove_bad([Letter|Corpus],NewCorpus) :- Letter == 32,remove_bad(Corpus,NewCorpus). /* SPACE */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 33,remove_bad(Corpus,NewCorpus). /* !     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 34,remove_bad(Corpus,NewCorpus). /* "     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 35,remove_bad(Corpus,NewCorpus). /* #     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 36,remove_bad(Corpus,NewCorpus). /* $     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 37,remove_bad(Corpus,NewCorpus). /* %     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 38,remove_bad(Corpus,NewCorpus). /* &     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 39,remove_bad(Corpus,NewCorpus). /* '     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 40,remove_bad(Corpus,NewCorpus). /* (     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 41,remove_bad(Corpus,NewCorpus). /* )     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 42,remove_bad(Corpus,NewCorpus). /* *     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 43,remove_bad(Corpus,NewCorpus). /* +     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 44,remove_bad(Corpus,NewCorpus). /* ,     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 45,remove_bad(Corpus,NewCorpus). /* -     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 46,remove_bad(Corpus,NewCorpus). /* .     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 47,remove_bad(Corpus,NewCorpus). /* /     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 58,remove_bad(Corpus,NewCorpus). /* :     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 59,remove_bad(Corpus,NewCorpus). /* ;     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 60,remove_bad(Corpus,NewCorpus). /* <     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 61,remove_bad(Corpus,NewCorpus). /* =     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 62,remove_bad(Corpus,NewCorpus). /* >     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 63,remove_bad(Corpus,NewCorpus). /* ?     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 64,remove_bad(Corpus,NewCorpus). /* @     */

remove_bad([Letter|Corpus],NewCorpus) :- Letter == 91,remove_bad(Corpus,NewCorpus). /* [     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 92,remove_bad(Corpus,NewCorpus). /* \     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 93,remove_bad(Corpus,NewCorpus). /* ]     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 94,remove_bad(Corpus,NewCorpus). /* ^     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 95,remove_bad(Corpus,NewCorpus). /* _     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 96,remove_bad(Corpus,NewCorpus). /* '     */

remove_bad([Letter|Corpus],NewCorpus) :- Letter == 123,remove_bad(Corpus,NewCorpus). /* {     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 124,remove_bad(Corpus,NewCorpus). /* |     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 125,remove_bad(Corpus,NewCorpus). /* }     */
remove_bad([Letter|Corpus],NewCorpus) :- Letter == 126,remove_bad(Corpus,NewCorpus). /* ~     */


remove_bad([Letter|Corpus],[Letter|Corpus]).




%
% default_features(?Category,?FeatureList).
%
% This predicate provides the default features for a lexical
% category. "FeatureList" is a prolog list that defines the
% feature list for "Category".
%


valid_cats(['(v)erb','(n)oun','(p)rep','(d)et']).

get_features(Letter,FeatureList) :- match_letter(Letter,Cat),
	                            default_features(Cat,FeatureList).


match_letter(v,verb).
match_letter(n,noun).
match_letter(p,prep).
match_letter(d,det).


default_features(verb   ,[cat:verb,lang:Language,sem:Sem,tense:Tense,inf:Infinitive,num:Number,person:Person,gender:Gender]).
default_features(noun   ,[cat:noun,lang:Language,sem:Sem,sound:Sound,type:Type,     num:Number,person:Person,gender:Gender]).
default_features(prep   ,[cat:prep,lang:Language,sem:Sem,type:Type]).
default_features(art    ,[cat:art, lang:Language,sem,Sem,sound:Sound,type:Type]).
default_features(adj    ,[cat:adj, lang:Language,sem:Sem,sound:Sound,num:Number]).
default_features(adv    ,[cat:adv, lang:Language,sem:Sem]).
