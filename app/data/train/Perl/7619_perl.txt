
prune_([X|L],[X|R]):-
delete_(X,L,W),
prune_(W,R).

prune_([],[]).
