mynumber(0).
mynumber(s(X)) :- mynumber(X).

myplus(0, X, X) :- mynumber(X).
myplus(s(X), Y, s(Z)) :- myplus(X, Y, Z).

myminus(X, Y, Z) :- myplus(Y, Z, X).

mytimes(0, X, 0) :- mynumber(X).
mytimes(s(X), Y, Z) :- mytimes(X, Y, XY), myplus(XY, Y, Z).

mydiv(X, X, s(0)).
mydiv(X, Y, s(Z)) :- myminus(X, Y, XY), mydiv(XY, Y, Z).

myexp(0, s(X), 0) :- mynumber(X).
myexp(s(X), 0, s(0)) :- mynumber(X).
myexp(X, s(N), Y) :- myexp(X, N, Z), mytimes(Z, X, Y).

myfact(0, s(0)).
myfact(s(X), Y) :- myfact(X, FX), mytimes(s(X), FX, Y).
