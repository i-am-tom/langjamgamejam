father("John", "Paul").
father("Mary", "Sarah").
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

childless(X) :- \+ parent(X, Y).

