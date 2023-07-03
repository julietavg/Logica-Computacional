
%% Regla que nos dice si el predicado elem(X,M) es verdadero. 
elem(X, [X|_]).
elem(X, [_|T]) :- elem(X, T).

%% Regla que nos diga si el predicado intersect(M,N) sea verdadero.
intersect(M, N) :- elem(X, M), elem(X, N).

%% Regla que nos diga si el predicado delete(X,L,NewL) es verdadero. 
delete(_, [], []).
delete(X, [X|T], T).
delete(X, [H|T], [H|NewT]) :- X \= H, delete(X, T, NewT).

%% Concatenación, función auxiliar:
concat([],L,L).
concat([X|L1],L2,[X|L3]) :- concat(L1,L2,L3).

%% Algoritmo de Hao Wang:
wang(G,D) :- intersect(G,D),!.
wang(G,D) :- elem(not(P), G), delete(not(P),G,NG), wang(NG,[P|D]), !.
wang(G,D) :- elem(not(P), D), delete(not(P),D,ND), wang([P|G],ND), !.
wang(G,D) :- elem(and(P,Q), G), delete(and(P,Q),G, NG), concat([P,Q], NG, NNG), wang(NNG, D), !.
wang(G,D) :- elem(or(P,Q), D), delete(or(P,Q),D, ND), concat([P,Q], ND, NND), wang(G, NND), !.
wang(G,D) :- elem(or(P,Q), G), delete(or(P,Q),G, NG), wang([P|NG], D), wang([Q|NG], D), !.
wang(G,D) :- elem(and(P,Q), D), delete(and(P,Q),D, ND), wang(G, [P|ND]), wang(G, [Q |ND]), !.

%% Regla para saber si el perdicado F es correcto a base de Hao wong:
valid(F) :- wang([],F).