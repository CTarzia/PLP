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
islas(_,_).

%%% EJERCICIO 2

% islasVecinas(+M, +I, -Is)
islasVecinas(_, _, _).

%%% EJERCICIO 3

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas(_, _, _,_).

%%% EJERCICIO 4

% caminoSimple(+M, +O, +D, -C)
caminoSimple(_, _, _, _).

%%% EJERCICIO 5

% mapa(+M)
mapa(_).

%%% EJERCICIO 6

% caminoHamiltoniano(+M, +O, +D, -C)
caminoHamiltoniano(_, _, _, _).

%%% EJERCICIO 7

% caminoHamiltoniano(+M, -C)
caminoHamiltoniano(_, _).

%%% Ejercicio 8

% caminoMinimo(+M, +O, +D, -C, -Distancia)
caminoMinimo(_, _, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(1). % ¡Actualizar!
testIslas(1) :- mapaEjemplo(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [papeete, tahiti, uturoa]).

cantidadTestsMapa(1). % ¡Actualizar!
testMapa(1) :- noMapa(NM), not(mapa(NM)).

cantidadTestsCaminos(3). % ¡Actualizar!
testCaminos(1) :- mapaEjemplo(Mapa), setof(C, caminoSimple(Mapa, uturoa, papeete, C), L), length(L, 2).
testCaminos(2) :- mapaEjemplo(Mapa), setof(C, caminoHamiltoniano(Mapa, uturoa, papeete, C), L), length(L, 1).
testCaminos(3) :- mapaEjemplo3(M),setof(C, caminoHamiltoniano(M, C), L), length(L, 8).


tests(islas) :- cantidadTestsIslas(M), forall(between(1,M,N), testIslas(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).

tests(todos) :-
  tests(islas),
  tests(mapa),
  tests(caminos).

tests :- tests(todos).
