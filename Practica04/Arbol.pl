%%%%% Árbol genealógico

% Hechos
hombre("Bart Simpson").
hombre("Homer Simpson").
hombre("Abraham J. Simpson").
hombre("Herbert Powel").
hombre("Clancy Bouvier").

mujer("Lisa Simpson").
mujer("Maggie Simpson").
mujer("Marge Simpson").
mujer("Ling").
mujer("Patty Bouvier").
mujer("Selma Bouvier").
mujer("Jacqueline Bouvier").
mujer("Mona Simpson").
mujer("Abbie").
mujer("Edwina").
mujer("???").

progenitor("Homer Simpson","Bart Simpson").
progenitor("Marge Simpson","Bart Simpson").
progenitor("Mona Simpson","Homer Simpson").
progenitor("Abraham J. Simpson","Homer Simpson").
progenitor("Abraham J. Simpson","Herbert Powel").
progenitor("Edwina","Abbie").
progenitor("Abraham J. Simpson","Abbie").
progenitor("Homer Simpson","Lisa Simpson").
progenitor("Marge Simpson","Lisa Simpson").
progenitor("Homer Simpson","Maggie Simpson").
progenitor("Marge Simpson","Maggie Simpson").
progenitor("Jacqueline Bouvier","Marge Simpson").
progenitor("Clancy Bouvier","Marge Simpson").
progenitor("Jacqueline Bouvier","Patty Bouvier").
progenitor("Clancy Bouvier","Patty Bouvier").
progenitor("Jacqueline Bouvier","Selma Bouvier").
progenitor("Clancy Bouvier","Selma Bouvier").
progenitor("Selma Bouvier","Ling").

pareja("Homer Simpson","Marge Simpson").
pareja("Abraham J. Simpson","Mona Simpson").
pareja("Abraham J. Simpson","Edwina").
pareja("Abraham J. Simpson","???").
pareja("Clancy Bouvier","Jacqueline Bouvier").
pareja("Marge Simpson","Homer Simpson").
pareja("Mona Simpson","Abraham J. Simpson").
pareja("Edwina","Abraham J. Simpson").
pareja("???","Abraham J. Simpson").
pareja("Jacqueline Bouvier","Clancy Bouvier").

% Reglas
padre(X, Y) :-
   progenitor(Y, X),
   hombre(Y).
    
madre(X, Y) :-
   progenitor(Y, X),
   mujer(Y).
    
hermanos(X, Y) :-
    padre(X, P),
    padre(Y, P),
    X \= Y.

hermano(X, Y) :-
    hermanos(Y, X),
    hombre(Y).

hermana(X, Y) :-
    hermanos(Y, X),
    mujer(Y).

esposo(Y, X) :-
    pareja(X, Y),
    hombre(X).

esposa(X, Y) :-
    pareja(X, Y),
    mujer(Y).

suegro(X, Y) :-
    pareja(X, Z),
    progenitor(Y, Z),
    hombre(Y).

suegra(X, Y) :-
    pareja(X, Z),
    progenitor(Y, Z),
    mujer(Y).

cuñada(X,Y):-
    ((pareja(Z, X),hermanos(Y, Z));
    (hermanos(Z, X),pareja(Y, Z))),
    mujer(Y).
    
cuñado(X,Y):-
    ((pareja(Z, X),hermanos(Y, Z));
    (hermanos(Z, X),pareja(Y, Z))),
    hombre(Y).

cuñados(X,Y):-
    cuñada(X,Y);
    cuñado(X,Y).

abuelo(X, Y) :-
    progenitor(Z, X),
    progenitor(Y, Z),
    hombre(Y).

abuela(X, Y) :-
    progenitor(Z, X),
    progenitor(Y, Z),
    mujer(Y).

nieto(X, Y) :-
    progenitor(X, Z),
    progenitor(Z, Y),
    hombre(Y).

nieta(X, Y) :-
    progenitor(X, Z),
    progenitor(Z, Y),
    mujer(Y).

tio(X, Y) :-
    progenitor(Z, X),
    hermanos(Z,Y),
    hombre(Y).

tia(X, Y) :-
    progenitor(Z, X),
    hermanos(Z,Y),
    mujer(Y).

prima(X,Y) :-
    progenitor(Z,X),
    hermanos(Z,W),
    progenitor(W,Y),    
    mujer(Y).

primo(X,Y) :-
    progenitor(Z,X),
    hermanos(Z,W),
    progenitor(W,Y),    
    hombre(Y).
