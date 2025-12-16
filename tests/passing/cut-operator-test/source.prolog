father("John", "Paul").
father("John", "Mark").
father("John", "Luke").

first-child(X, Y) :- !, father(X, Y).

