father("John", "Paul").
father("Paul", "Mark").
grandfather(X, Y) :- father(X, Z), father(Z, Y).

