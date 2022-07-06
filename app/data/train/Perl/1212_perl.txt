wizard(ron).
hasWand(harry).
quidditchPlayer(harry).
wizard(X) :- hasBroom(X), hasWand(X).
hasBroom(X) :- quidditchPlayer(X).

% ?- wizard(ron).
% true.

% ?- witch(ron).
% ERROR

% ?- wizard(hermione).
% false.

% ?- witch(hermione).
% ERROR

% ?- wizard(harry).
% true.

% ?- wizard(Y).
% Y = harry;
% Y = ron.

% witch(Y).
% ERROR.
