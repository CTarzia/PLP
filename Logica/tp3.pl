%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEFINICIONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mapaEjemplo([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10)]).
      
mapaEjemplo2([
      ruta(valitupu, funafuti, 30),
      ruta(valitupu, savave, 10),
      ruta(savave, valitupu, 20),
      ruta(savave, funafuti, 10),
      ruta(funafuti, valitupu, 30),
      ruta(funafuti, savave, 20)]).
      
mapaEjemplo3([
      ruta(nui, valitupu, 50),
      ruta(nui, savave, 40),
      ruta(valitupu, funafuti, 30),
      ruta(valitupu, savave, 10),
      ruta(savave, valitupu, 20),
      ruta(savave, funafuti, 10),
      ruta(savave, nui, 50),
      ruta(funafuti, valitupu, 30),
      ruta(funafuti, savave, 20)]).
      
noMapa([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(uturoa, tahiti, 20)]).

noMapa2([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10),
      ruta(mururoa,rikitea,20),
      ruta(rikitea,mururoa,20)]).
      
noMapa3([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(tahiti, tahiti, 10),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10)]).
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EJERCICIOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EJERCICIO 1

% islas(+M, -Is)
agregarIsla(Islas, I, [I|Islas]) :- not(member(I,Islas)).
agregarIsla(Islas, I, Islas) :- member(I,Islas). 

islas([],[]).
islas([ruta(A,B,_)|ElResto], Is) :- 
    islas(ElResto,IsRec),
    agregarIsla(IsRec, A , IsConA),
    agregarIsla(IsConA, B, Is).

%%% EJERCICIO 2

% islasVecinas(+M, +I, -Is)
islasVecinas([], _, []).
islasVecinas([ruta(A,B,_)|ElResto], A, Is) :-
    islasVecinas(ElResto, A, IsRec),
    agregarIsla(IsRec, B, Is).
islasVecinas([ruta(A,_,_)|ElResto], C, Is) :-
    A \= C,
    islasVecinas(ElResto, C, Is).

%%% EJERCICIO 3

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas(M, I1, I2, N) :- member(ruta(I1,I2,N), M).

%%% EJERCICIO 4

% caminoSimple(+M, +O, +D, -C)
esVecino(M, Isla1, Isla2) :- member(ruta(Isla1, Isla2, _), M). 

esCaminoValido(M, [Isla1, Isla2]) :- esVecino(M, Isla1, Isla2).
esCaminoValido(M, [Isla1, Isla2|ElResto]) :-
    esVecino(M, Isla1, Isla2),
    esCaminoValido(M, [Isla2|ElResto]).

caminoSimple(M, O, D, [O|C]) :-
    islas(M,Is),
    length(Is,CantIs),
    between(2,CantIs,LargoC),
    length([O|C], LargoC),
    esCaminoValido(M, [O|C]),
    is_set([O|C]),
    last(C,D).
    %%% TODO: estudiar reversibilidad

%%% EJERCICIO 5

todasLasIslasAlcanzables(M) :-
    islas(M, Islas),
    forall(
      (member(A, Islas),
      member(B, Islas), A\=B),
      caminoSimple(M, A, B, _)
    ).

noHayRutaDeIslaASiMisma(M) :- not(member(ruta(I,I,_), M)).
noHayRutasRepetidas(M) :-
    not( 
        (
            member(ruta(A,B,D1), M),
            member(ruta(A,B,D2), M),
            D1 \= D2
        )    
    ), is_set(M).

% mapa(+M)
mapa(M) :-
    todasLasIslasAlcanzables(M),
    noHayRutaDeIslaASiMisma(M),
    noHayRutasRepetidas(M).

%%% EJERCICIO 6

% caminoHamiltoniano(+M, +O, +D, -C)
caminoHamiltoniano(M, O, D, C) :- caminoSimple(M, O, D, C), islas(M, Is), same_length(Is, C).

%%% EJERCICIO 7

% caminoHamiltoniano(+M, -C)
caminoHamiltoniano(M, C) :-
    islas(M, Is), member(A,Is), member(B,Is), A \= B, caminoHamiltoniano(M, A, B, C).

%%% Ejercicio 8

distancia(M, [I1,I2], D) :- member(ruta(I1,I2,D), M).
distancia(M, [I1,I2|Resto], D) :-
    distancia(M, [I2|Resto], DRec),
    member(ruta(I1,I2,DVec), M),
    D is DRec + DVec.

hayCaminoMejor(M, O, D, Dist) :-
    caminoSimple(M, O, D, C),
    distancia(M, C, OtraDist),
    OtraDist < Dist.

% caminoMinimo(+M, +O, +D, -C, -Distancia)
caminoMinimo(M, O, D, C, Dist) :-
    caminoSimple(M, O, D, C),
    distancia(M, C, Dist),
    not(hayCaminoMejor(M, O, D, Dist)).
    %%% TODO: estudiar reversibilidad

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(5).
testIslas(1) :-
    agregarIsla([isla1, isla2, isla3], isla4, NuevasIslas),
    length(NuevasIslas, 4),
    sort(NuevasIslas, [isla1, isla2, isla3, isla4]).
testIslas(2) :-
    agregarIsla([isla1, isla2, isla3], isla3, NuevasIslas),
    length(NuevasIslas, 3),
    sort(NuevasIslas, [isla1, isla2, isla3]).
testIslas(3) :-
    mapaEjemplo(Mapa),
    islas(Mapa, Islas),
    length(Islas, 3),
    sort(Islas, [papeete, tahiti, uturoa]).
testIslas(4) :-
    mapaEjemplo2(Mapa),
    islas(Mapa, Islas),
    length(Islas, 3),
    sort(Islas, [funafuti, savave, valitupu]).
testIslas(5) :-
    mapaEjemplo3(Mapa),
    islas(Mapa, Islas),
    length(Islas, 4),
    sort(Islas, [funafuti, nui, savave, valitupu]).

cantidadTestsIslasVecinas(3).
testIslasVecinas(1) :-
    mapaEjemplo(Mapa),
    islasVecinas(Mapa, uturoa, Is),
    length(Is, 2),
    sort(Is, [papeete, tahiti]).
testIslasVecinas(2) :-
    mapaEjemplo3(Mapa),
    islasVecinas(Mapa, nui, Is),
    length(Is, 2),
    sort(Is, [savave, valitupu]).
testIslasVecinas(3) :-
    mapaEjemplo3(Mapa),
    islasVecinas(Mapa, savave, Is),
    length(Is, 3),
    sort(Is, [funafuti, nui, valitupu]).

cantidadTestsDistanciaVecinas(0). % ¡Actualizar!

cantidadTestsMapa(1). % ¡Actualizar!
testMapa(1) :- noMapa(NM), not(mapa(NM)).

cantidadTestsCaminos(3). % ¡Actualizar!
testCaminos(1) :- mapaEjemplo(Mapa), setof(C, caminoSimple(Mapa, uturoa, papeete, C), L), length(L, 2).
testCaminos(2) :- mapaEjemplo(Mapa), setof(C, caminoHamiltoniano(Mapa, uturoa, papeete, C), L), length(L, 1).
testCaminos(3) :- mapaEjemplo3(M),setof(C, caminoHamiltoniano(M, C), L), length(L, 8).


tests(islas) :- cantidadTestsIslas(M), forall(between(1,M,N), testIslas(N)).
tests(islasVecinas) :- cantidadTestsIslasVecinas(M), forall(between(1,M,N), testIslasVecinas(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).

tests(todos) :-
  tests(islas),
  tests(islasVecinas),
  tests(mapa),
  tests(caminos).

tests :- tests(todos).
