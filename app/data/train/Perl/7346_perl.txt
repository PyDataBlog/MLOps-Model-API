point1d(X) :- integer(X).
point2d(point1d(X), point1d(Y)) :- point1d(X), point1d(Y).
point(point1d(X)) :- point1d(X).
point(point2d(X, Y)) :- point2d(X, Y).
graph_obj(X) :- point(X).

translate(point1d(X), point1d(X1), Z) :- point1d(X),
	point1d(X1),
	XTranslated is X + X1,
	Z = point1d(XTranslated).

translate(point2d(point1d(X), point1d(Y)), point2d(point1d(X1), point1d(Y1)), Z) :-
	point2d(point1d(X), point1d(Y)),
	point2d(point1d(X1), point1d(Y1)),
	XTranslated is X + X1,
	YTranslated is Y + Y1,
	Z = point2d(point1d(XTranslated), point1d(YTranslated)).
