/* Problem 1 */
odd_list(X, [], Y) :-
    Y >= X.
odd_list(X, [Y|L], Y) :-
    X > Y, Z is Y+2, odd_list(X, L, Z).
odd_list(X, L) :-
    odd_list(X, L, 1).

/* Problem 2 */
nexttri(X, Y, Y, _) :-
    X < Y.
nexttri(X, Y, A, B) :-
    X >= A, P is A+B, Q is B+1, nexttri(X, Y, P, Q).
nexttri(X, Y) :-
    nexttri(X, Y, 1, 2).

/* Problem 3 */
member(E, [E|_]).
member(E, [_|X]) :-
    member(E, X).

list_diff([], _,[]).
list_diff([E|X], L, Y) :-
    member(E, L), list_diff(X, L, Y).
list_diff([E|X], L, [E|Y]) :-
    not(member(E, L)), list_diff(X, L, Y).

/* Problem 4 */
remove_duplicate([], []).
remove_duplicate(X, [E|Y]) :-
    member(E, Y), remove_duplicate(X, Y).
remove_duplicate([E|X], [E|Y]) :-
    not(member(E, Y)), remove_duplicate(X, Y).

list_merge(X, Y, Z) :-
    append(X, Y, L), remove_duplicate(Z, L).

/* Problem 5 */
if_is_true(true).
if_is_true(X):-
    X = true, !, fail.
if_is_true(and(X, Y)) :-
    and(X, Y).
if_is_true(or(X, Y)) :-
    or(X, Y).

and(X, Y) :-
    if_is_true(X), if_is_true(Y).

or(X, _) :-
    if_is_true(X).
or(_, X) :-
    if_is_true(X).
