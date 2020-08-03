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
islaEnMapa(Mapa, Isla) :- member(ruta(Isla, _, _), Mapa).
islaEnMapa(Mapa, Isla) :- member(ruta(_, Isla, _), Mapa).
islas(Mapa,Islas) :- setof(Isla, islaEnMapa(Mapa, Isla), Islas).

%%% EJERCICIO 2

% islasVecinas(+M, +I, -Is)
esVecino(M, Isla1, Isla2) :- member(ruta(Isla1, Isla2, _), M).
islasVecinas(Mapa, I, Is) :- setof(Vec, esVecino(Mapa, I, Vec), Is).

%%% EJERCICIO 3

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas(M, I1, I2, N) :- member(ruta(I1,I2,N), M).

%%% EJERCICIO 4
% caminoSimple(+Mapa, +Origen, +Destino, ?Camino, +VerticesIslasYaVisitados)
caminoSimple(M, O, D, [O, D], V) :-
    not(member(O, V)),
    not(member(D, V)),
    esVecino(M, O, D).

caminoSimple(M, O, D, [O|C], V) :-
    not(member(O, V)),
    esVecino(M, O, S),
    caminoSimple(M, S, D, C, [O|V]).

% Analisis de reversibilidad:
%%%
% Si M no viene instanciada, el programa se tilda.
% Esto sucede ya que islas(M, Islas) genera una rama infinita.
% islas(M, Islas) implica resolver setof con predicado islaEnMapa(Mapa, Isla)
% Si islaEnMapa(Mapa, Isla) se llama sin ningun parametro instanciado,
% en la primer clausula reduce por member(ruta(Isla, _, _), Mapa),
% por lo que Mapa se instancia como una lista.
% Aca se tilda, ya que setof llamando a islaEnMapa instancia
% Mapa = [ruta(Isla, _, _)], luego
% Mapa = [_, ruta(Isla, _, _)], luego
% Mapa = [_, _, ruta(Isla, _, _)], y asi sigue.
% Por eso, M debe venir instanciada
%%%
% Si M está instanciada y O o D (son analogos) no están instanciadas,
% islas(M, Islas) instancia Islas con la lista de todas las listas
% y luego los member(O,Islas), member(D,Islas)
% instancian O y D si no venian instanciados.
% Si O o D venian instanciados, member(O,Islas), member(D,Islas)
% solo chequea que estén en el listado de Islas,
% haciendo que se corte en False si las O o D vienen instanciadas con islas no existentes.
%%%
% Notar que O y D se instancian antes de que se haga cualquier uso de C.
% Unas vez instanciadas O y D (o si ya venian instanciadas), y con M instanciada,
% si C no viene instanciada, la primer clausula de caminoSimple/5
% va a instanciarlo como [O, D] y va a terminar satisfactoriamente si y solo si
% O es vecino de D en M.
% La segunda clausula de caminoSimple/5 va a instanciar S con los vecinos de O,
% instanciar C con la lista con primer elemento O,
% y seguir completando C con los caminos de S a D (ambos instanciados).
% Si C ya venia instanciada como [], fallará al no poder unificar
% con ninguna de las clausulas de caminoSimple/5
% (la primera clausula pide C de dos elementos y la segunda de al menos un elemento).
% Si C venia instanciada con un solo elemento, unificara con la segunda clausula,
% instanciara S con un vecino de O,
% pero luego llamara recursivamente a caminoSimple/5 con un nuevo C vacio, que fallara.
% Si C venia instanciada con dos elementos, unificara con la primer clausula sii C = [O,D]
% y verificara que O sea vecino de D.
% Luego unificara con la segunda clausula, instanciara S con un vecino de O,
% y llamará recursivamente a caminoSimple/5 con un nuevo C de un solo elemento,
% que como dijimos recien, siempre falla.
% Si C tiene mas de 2 elementos, no unificara con la primer clausula pero si con la segunda,
% si y solo si C tiene como primer elemento a O.
% Esta instanciara S con un vecino de O y llamara recursivamente a caminoSimple/5
% con la cola de C y con V teniendo a O, para evitar ciclos.
% Las llamadas recursivas seguiran chequeando lo mismo que dijimos dependiendo el tamaño de C,
% agregando el chequeo anti-ciclos con V no vacio.

% caminoSimple(+M, ?O, ?D, ?C)
caminoSimple(M, O, D, C) :-
    islas(M, Islas),
    member(O, Islas),
    member(D, Islas),
    % Se buscan caminos simples de O a D empezando sin haber pasado por ningun vertice isla
    caminoSimple(M, O, D, C, []).

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
  findall(ruta(A, B), esVecino(M, A, B), Vecinos),
  is_set(Vecinos).

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

% Analisis de reversibilidad:
%%%
% Si M no viene instanciada, el programa se tilda.
% Esto sucede ya que primero se intenta resolver caminoSimple(M, O, D, C),
% y este primero intenta resolver islas(M, Islas),
% que genera una rama infinita.
% La justificacion de esto se puede leer en el analisis de reversibilidad de caminoSimple/4.
% Por esto, siempre debe venir instanciada.
%%%
% Si O o D no vienen instanciadas, al resolver caminoSimple(M, O, D, C)
% son instanciadas con islas que pertenecen a M por el member(O, Islas) y member(D, Islas).
% Notar que O y D se instancian antes de que se haga cualquier uso de C.
% Nuevamente, si C no venia instanciada, sera instanciada con todos los caminos posibles de O a D
% en caminoSimple. Como se instancia C si no venia instanciada esta explicado en el analisis
% de reversibilidad de caminoSimple/4.
% Una vez resuelto caminoSimple(M, O, D, C), estas cuatro variables estaran instanciadas o bien
% porque ya venian instanciadas o porque caminoSimple/4 las instancio (en el caso de O, D y C).
% Ademas, sabemos que estan instanciadas con algo valido
% (O y D son islas en M, C es un camino de O a D con rutas validas de M).
% Luego, si Dist no venia instanciada, distancia(M, C, Dist) la instanciara sumando
% la distancia de las distintas rutas del camino C en el mapa M.
% Para que esto suceda, si C tiene dos elementos solo unifica con la primer clausula de distancia/3,
% unificando la Distancia con la que aparece en la ruta de M entre esos dos elementos.
% Si C tiene mas de dos elementos, solo unificara con la segunda clausula de distancia/3,
% que resolvera el problema de la distancia (DRec) en la cola de C
% y luego a esta distancia le sumara la distancia entre las primeras dos islas
% y unificara Dist con el resultado de la suma (con el 'is').
% Notar que C no puede tener menos de dos elementos ya que fue instanciado o chequeado
% anteriormente (en caminoSimple(M, O, D, C)) para que sea un camino valido de O a D.
% Si Dist ya venia instanciada, distancia(M, C, Dist)
% solo chequeara que tenga el valor correcto
% con el member en la primer clausula si C tiene dos elementos o
% con el 'is' en la segunda clausula si C tiene mas de dos elementos.
%%%
% caminoMinimo(+M, ?O, ?D, ?C, ?Distancia)
caminoMinimo(M, O, D, C, Dist) :-
    caminoSimple(M, O, D, C),
    distancia(M, C, Dist),
    not(hayCaminoMejor(M, O, D, Dist)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(5).
testIslas(1) :-
    mapaEjemplo(Mapa),
    islaEnMapa(Mapa, papeete).
testIslas(2) :-
    mapaEjemplo2(Mapa),
    islaEnMapa(Mapa, savave).
testIslas(3) :-
    mapaEjemplo3(Mapa),
    not(islaEnMapa(Mapa, tahiti)).
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

cantidadTestsCaminoSimple(6).
testCaminoSimple(1) :- % Podemos mirar todos los caminos simples
    mapaEjemplo(Mapa),
    setof(Camino, A^B^caminoSimple(Mapa, A, B, Camino), Caminos),
    length(Caminos, 12).
testCaminoSimple(2) :- % Podemos ver si un camino que es simple existe
    mapaEjemplo3(Mapa),
    not(caminoSimple(Mapa, _, _, [funafuti, valitupu, nui])).
testCaminoSimple(3) :- % Podemos ver si un camino que existe es simple
    mapaEjemplo3(Mapa),
    caminoSimple(Mapa, _, _, [nui, valitupu, funafuti]),
    caminoSimple(Mapa, _, _, [funafuti, valitupu]),
    not(caminoSimple(Mapa, _, _, [nui, valitupu, funafuti, valitupu])).
testCaminoSimple(4) :- % Podemos extraer informacion de un camino simple
    mapaEjemplo3(Mapa),
    caminoSimple(Mapa, Origen, Destino, [nui, valitupu, funafuti]),
    Origen = nui,
    Destino = funafuti.
testCaminoSimple(5) :- % Podemos conseguir los caminos simples entre dos puntos
    mapaAburrido(Mapa),
    setof(Camino, caminoSimple(Mapa, a, d, Camino), Caminos),
    length(Caminos, 3).
testCaminoSimple(6) :- % Podemos ver que todos los caminos de "un paso" son simples
    mapaEjemplo3(Mapa),
    forall(esVecino(Mapa, A, B), caminoSimple(Mapa, A, B, [A, B])).

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

cantidadTestsHamiltoniano(4).
testHamiltoniano(1) :- % Se puede completar el camino hamiltoniano
  mapaEjemplo(Mapa),
  setof(Camino, caminoHamiltoniano(Mapa, uturoa, tahiti, Camino), Caminos),
  length(Caminos, 1).
testHamiltoniano(2) :- % Se pueden conocer todos los caminos hamiltonianos
  mapaEjemplo(Mapa),
  setof(Camino, caminoHamiltoniano(Mapa, Camino), Caminos),
  length(Caminos, 6).
testHamiltoniano(3) :- % Hay grafos sin caminos hamiltonianos
  mapaAburrido(Mapa),
  not(caminoHamiltoniano(Mapa, _)).
testHamiltoniano(4) :- % Hay grafos que tienen más de un camino hamiltoniano de A a B
  mapaEjemplo3(Mapa),
  setof(Camino, caminoHamiltoniano(Mapa, nui, funafuti, Camino), Caminos),
  length(Caminos, 2).

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
tests(caminoSimple) :- cantidadTestsCaminoSimple(M), forall(between(1,M,N), testCaminoSimple(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).
tests(hamiltoniano) :- cantidadTestsHamiltoniano(M), forall(between(1,M,N), testHamiltoniano(N)).
tests(distancia) :- cantidadTestsDistancia(M), forall(between(1,M,N), testDistancia(N)).
tests(hayCaminoMejor) :- cantidadTestsHayCaminoMejor(M), forall(between(1,M,N), testHayCaminoMejor(N)).
tests(caminoMinimo) :- cantidadTestsCaminoMinimo(M), forall(between(1,M,N), testCaminoMinimo(N)).

tests(todos) :-
  tests(islas),
  tests(islasVecinas),
  tests(distanciaVecinas),
  tests(caminoSimple),
  tests(mapa),
  tests(caminos),
  tests(hamiltoniano),
  tests(distancia),
  tests(hayCaminoMejor).
  tests(caminoMinimo).

tests :- tests(todos).
