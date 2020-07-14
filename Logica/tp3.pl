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

% Un mapa con múltiples caminos mínimos de A a D
mapaAburrido([
      ruta(a, b1, 1),
      ruta(a, b2, 1),
      ruta(a, b3, 1),
      ruta(b3, c, 1),
      ruta(b1, d, 2),
      ruta(b2, d, 2),
      ruta(c, d, 1),
      ruta(d, a, 1)]).
      
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

% El mapa M debe venir siempre instanciado, sino islas(M, Is) se cuelga.
% Luego, tanto O, como D y C pueden o no venir.
% En el caso que no venga ninguno, va a instancias todos los
% caminos simples posibles en C, el origen de C en O y el destino de C en D.
% Si vienen O y D instancias pero C no, se instancia C con todos
% los caminos simples posibles.
% Si viene solo O instanciado, instanciará todos los C caminos simples
% con origen en O e instanciará D con el destino de C.
% Asi sigue con las distintas combinaciones.
% caminoSimple(+M, ?O, ?D, ?C)
caminoSimple(M, O, D, [O|C]) :-
    islas(M,Is),
    length(Is,CantIs),
    % Probamos caminos de largo 2 hasta CantidadDeIslas
    between(2,CantIs,LargoC),
    length([O|C], LargoC),
    esCaminoValido(M, [O|C]),
    is_set([O|C]),
    last(C,D).

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

cantidadTestsDistanciaVecinas(3).
testDistanciaVecinas(1) :-
    mapaEjemplo(Mapa),
    distanciaVecinas(Mapa, tahiti, papeete, 20).
testDistanciaVecinas(2) :-
    mapaEjemplo(Mapa),
    distanciaVecinas(Mapa, papeete, tahiti, 10).
testDistanciaVecinas(3) :-
    mapaEjemplo3(Mapa),
    distanciaVecinas(Mapa, nui, valitupu, 50).

cantidadTestsMapa(16).
testMapa(1) :- noMapa(NM), not(mapa(NM)).
testMapa(2) :- noMapa2(NM), not(mapa(NM)).
testMapa(3) :- noMapa3(NM), not(mapa(NM)).
testMapa(4) :- mapaEjemplo(Mapa), mapa(Mapa).
testMapa(5) :- mapaEjemplo2(Mapa), mapa(Mapa).
testMapa(6) :- mapaEjemplo3(Mapa), mapa(Mapa).
%% El noMapa tiene el problema de rutas repetidas.
testMapa(7) :- noMapa(NM), todasLasIslasAlcanzables(NM).
testMapa(8) :- noMapa(NM), noHayRutaDeIslaASiMisma(NM).
testMapa(9) :- noMapa(NM), not(noHayRutasRepetidas(NM)).
%% El noMapa2 tiene el problema de tener dos componentes conexas.
testMapa(10) :- noMapa2(NM), not(todasLasIslasAlcanzables(NM)).
testMapa(11) :- noMapa2(NM), noHayRutaDeIslaASiMisma(NM).
testMapa(12) :- noMapa2(NM), noHayRutasRepetidas(NM).
%% El noMapa3 tiene una ruta de una isla a si mismas.
testMapa(13) :- noMapa3(NM), todasLasIslasAlcanzables(NM).
testMapa(14) :- noMapa3(NM), not(noHayRutaDeIslaASiMisma(NM)).
testMapa(15) :- noMapa3(NM), noHayRutasRepetidas(NM).
%% El mapaAburrido tiene varios caminos mínimos de A a D
testMapa(16) :- mapaAburrido(Mapa), mapa(Mapa).

cantidadTestsCaminos(3). % ¡Actualizar!
testCaminos(1) :- mapaEjemplo(Mapa), setof(C, caminoSimple(Mapa, uturoa, papeete, C), L), length(L, 2).
testCaminos(2) :- mapaEjemplo(Mapa), setof(C, caminoHamiltoniano(Mapa, uturoa, papeete, C), L), length(L, 1).
testCaminos(3) :- mapaEjemplo3(M),setof(C, caminoHamiltoniano(M, C), L), length(L, 8).

cantidadTestsDistancia(3).
testDistancia(1) :- % Funciona con caminos no mínimos
  mapaEjemplo(Mapa), distancia(Mapa, [uturoa, tahiti], 50).
testDistancia(2) :- % Funciona con caminos no mínimos
  mapaEjemplo(Mapa), distancia(Mapa, [uturoa, tahiti, uturoa], 80).
testDistancia(3) :- % Funciona con caminos largos
  mapaEjemplo3(Mapa), distancia(Mapa, [nui, valitupu, savave, funafuti], 70).

cantidadTestsHayCaminoMejor(2).
testHayCaminoMejor(1) :- % No todos los caminos son mínimos
  mapaEjemplo(Mapa), hayCaminoMejor(Mapa, uturoa, tahiti, 50).
testHayCaminoMejor(2) :- % No hay caminos mejores que los mínimos
  mapaEjemplo(Mapa), not(hayCaminoMejor(Mapa, uturoa, tahiti, 30)).

cantidadTestsCaminoMinimo(6).
testCaminoMinimo(1) :- % Existe un camino mínimo
  mapaEjemplo(Mapa), caminoMinimo(Mapa, uturoa, tahiti, Camino, Distancia),
  length(Camino, 3), Distancia is 30.
testCaminoMinimo(2) :- % No existen caminos menores
  mapaEjemplo(Mapa), not((
    caminoMinimo(Mapa, uturoa, tahiti, _, Distancia), Distancia < 30
  )).
testCaminoMinimo(3) :- % El camino existe, pero no es mínimo
  mapaEjemplo(Mapa), not(caminoMinimo(Mapa, uturoa, tahiti, [uturoa, tahiti], 50)).
testCaminoMinimo(4) :- % El camino no existe, pero sería mínimo
  mapaEjemplo(Mapa), not(caminoMinimo(Mapa, uturoa, tahiti, [uturoa, tahiti], 20)).
testCaminoMinimo(5) :- % Hay más de un camino mínimo
  mapaAburrido(Mapa),
  caminoMinimo(Mapa, a, d, Camino1, Distancia), length(Camino1, 4),
  caminoMinimo(Mapa, a, d, Camino2, Distancia), length(Camino2, 3).
testCaminoMinimo(6) :- % Hay 3 caminos mínimos posibles!
  mapaAburrido(Mapa),
  setof(Camino, caminoMinimo(Mapa, a, d, Camino, _), Soluciones), length(Soluciones, 3).

tests(islas) :- cantidadTestsIslas(M), forall(between(1,M,N), testIslas(N)).
tests(islasVecinas) :- cantidadTestsIslasVecinas(M), forall(between(1,M,N), testIslasVecinas(N)).
tests(distanciaVecinas) :- cantidadTestsDistanciaVecinas(M), forall(between(1,M,N), testDistanciaVecinas(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).
tests(distancia) :- cantidadTestsDistancia(M), forall(between(1,M,N), testDistancia(N)).
tests(hayCaminoMejor) :- cantidadTestsHayCaminoMejor(M), forall(between(1,M,N), testHayCaminoMejor(N)).
tests(caminoMinimo) :- cantidadTestsCaminoMinimo(M), forall(between(1,M,N), testCaminoMinimo(N)).

tests(todos) :-
  tests(islas),
  tests(islasVecinas),
  tests(distanciaVecinas),
  tests(mapa),
  tests(caminos),
  tests(distancia),
  tests(hayCaminoMejor).
  tests(caminoMinimo).

tests :- tests(todos).
