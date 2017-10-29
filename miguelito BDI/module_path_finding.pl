:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).

% Predicado utilizado para escribir por consola la estructura de un nodo
% de la frontera
escribirCostoNodo(nodo(_ID,CostoCamino,_Camino,Heuristica)):-
    Costo is CostoCamino + Heuristica,
    write(' '),write(Costo),write(', ').

escribirFrontera(Frontera):-
    writeln('Frontera ordenada:'),
    write('['),
    forall(member(Nodo,Frontera),escribirCostoNodo(Nodo)),
    writeln(']'),nl.



%Recorre una frontera
%Si encuentra al Nodo, se queda con el mejor
%Si no encuentra al Nodo, lo agrega al final
reemplazarPorMejor(Nodo,[],[Nodo]).

reemplazarPorMejor(nodo(ID,Costo1,Camino1,H1),
                   [nodo(ID,Costo2,_Camino2,_H2)|RestoF],
                   [nodo(ID,Costo1,Camino1,H1)|RestoF]
                  ):-
	Costo1 =< Costo2.

reemplazarPorMejor(nodo(ID,Costo1,_Camino1,_H1),
                   [nodo(ID,Costo2,Camino2,H2)|RestoF],
                   [nodo(ID,Costo2,Camino2,H2)|RestoF]
                  ):-
	Costo1 > Costo2.


reemplazarPorMejor(nodo(ID1,Costo1,Camino1,H1),
                   [nodo(ID2,Costo2,Camino2,H2)|RestoF],
                   [nodo(ID2,Costo2,Camino2,H2)|FFinal]
                 ):-
	ID1 \= ID2,
	reemplazarPorMejor(nodo(ID1,Costo1,Camino1,H1),RestoF, FFinal).




%Selecciona al primer nodo de la frontera
%Retorna la frontera sin el primer nodo
seleccionar(Nodo,[Nodo|RestoFrontera],RestoFrontera).


% Obtiene los vecinos de un nodo que no estan en la lista de nodos
% visitados
generarVecinos([],Nodo,Vecinos):-
    node(Nodo,_Pos1,Vecinos).

generarVecinos(Visitados,Nodo,VecinosSinRepetir):-
    node(Nodo,_Pos1,Vecinos),
    findall([V,C],(member([V,C],Vecinos),not(member(V,Visitados))),VecinosSinRepetir).


% Ordena una lista de distancias (que son las heuristicas de un nodo a
% las metas)
ordenarDistancias(DistanciasDesordenadas,DistanciasOrdenadas):-
    quick_sortNumeros(DistanciasDesordenadas,DistanciasOrdenadas).


minimaDistancia(_Elem,[],[MinimaDistancia],MinimaDistancia).

minimaDistancia(VectorActual,[IDNodoMeta|RestoNodosMeta],[],MinDistFinal):-
    node(IDNodoMeta,VectorMeta,_Ady),
    distance(VectorActual,VectorMeta,NuevaDist),
    minimaDistancia(VectorActual,RestoNodosMeta,[NuevaDist],MinDistFinal),!.

minimaDistancia(VectorActual,[IDNodoMeta|RestoNodosMeta],[MinDistVieja],MinDistFinal):-
    node(IDNodoMeta,VectorMeta,_Ady),
    distance(VectorActual,VectorMeta,NuevaDist),
    NuevaDist =< MinDistVieja,
    minimaDistancia(VectorActual,RestoNodosMeta,[NuevaDist],MinDistFinal),!.

minimaDistancia(VectorActual,[_IDNodoMeta|RestoNodosMeta],[MinDistVieja],MinDistFinal):-
    minimaDistancia(VectorActual,RestoNodosMeta,[MinDistVieja],MinDistFinal).




% Obtiene las distancias de un nodo a todos los nodos que tienen un
% tesoro (metas).
% La heuristica elegida es Euclidiana
obtenerDistancias(_VectorActual,[],MinimaDistancia,MinimaDistancia).

obtenerDistancias(VectorActual,[IDNodoMeta|RestoNodosMeta],DistanciaAnterior,MinimaDistancia):-
    writeln('Entro a Obtener distancias con:'),
    writeln(VectorActual),writeln(IDNodoMeta),writeln(DistanciaAnterior),writeln(MinimaDistancia),
    node(IDNodoMeta,VectorMeta,_Ady),
    distance(VectorActual,VectorMeta,NuevaDistancia),

    minimo(NuevaDistancia,DistanciaAnterior,MinimaDistancia),

    writeln('OD| Obteniendo la mejor distancia:'),
    write('OD| Distancia menor anterior: '),writeln(DistanciaAnterior),
    write('OD| Nueva distancia: '),writeln(NuevaDistancia),
    write('OD| Nueva distancia menor: '),writeln(MinimaDistancia),

    obtenerDistancias(VectorActual,RestoNodosMeta,[MinimaDistancia],MinimaDistancia).

%Obtiene la mejor heuristica para un nodo
%Elige la primer heuristica de una lista de heuristica ordenadas
calcularHeuristica(IDNodo,MenorDistancia):-

    node(IDNodo,VectorNodo,_Ady),
    findall(IDNodoMeta,at([gold,_Nombre],IDNodoMeta),IDNodosMeta),
    minimaDistancia(VectorNodo,IDNodosMeta,[],MenorDistancia).

%Ordena los nodos de la frontera llamando a quick_sortNodos
%f()=g()+h()
%g()=Costo del Camino
%h()=Heuristica
ordenar_por_f(FronteraDesordenada,FronteraOrdenada):-
    quick_sortNodos(FronteraDesordenada,FronteraOrdenada).


%Agrega un nodo a la lista de visitados
agregarVisitado(Nodo,[],[Nodo]).

agregarVisitado(Nodo,Visitados,VisitadosConNodo):-
    append([Nodo],Visitados,VisitadosConNodo).


% Recorre una lista de Nodos vecinos de la forma [IDNodo,CostoPaso] y
% utiliza el predicado "actualizarFrontera" para agregar de a un Nodo
% vecino a la frontera de Nodos de la forma
% nodo(ID,CostoCamino,Camino,Heuristica).
% Utiliza el Nodo padre para calcular el camino y el costo del camino
% del Nodo vecino
% Caso Base:
obtenerFrontera(_NodoActual %Nodo padre
               ,[] %Lista de vecinos (caso base)
               ,Frontera %Frontera de nodos
               ,Frontera). %Frontera de nodos
%Caso Base:
obtenerFrontera(NodoPadre %Nodo padre
               ,[[IDNodoVecino,CostoPaso]|RestoVecinos] %Lista vecinos con un vecino y el resto de la lista
               ,[] %Frontera inicial vacia (caso base)
               ,FronteraNueva):- %Frontera final con el primer vecino

    %writeln('OB| Caso base frontera vacia'),
    actualizarFrontera(NodoPadre,IDNodoVecino,CostoPaso,[],FronteraPasoMedio),
    %writeln('OB| Caso base frontera vacia agregue el primer vecino'),

    obtenerFrontera(NodoPadre,RestoVecinos,FronteraPasoMedio,FronteraNueva).

%Caso recursivo:
obtenerFrontera(NodoPadre %Nodo padre
               ,[[IDNodoVecino,CostoPaso]|RestoVecinos] %Lista de vecinos con un vecino y el resto de la lista
               ,FronteraVieja %Frontera inicial
               ,FronteraNueva %Frontera final con el primer vecino
               ):-
    %writeln('OB| Caso recursivo'),
    actualizarFrontera(NodoPadre,IDNodoVecino,CostoPaso,FronteraVieja,FronteraPasoMedio),
    %writeln('OB| Caso recursivo agregue el primer vecino'),
    %writeln(FronteraPasoMedio),

    obtenerFrontera(NodoPadre,RestoVecinos,FronteraPasoMedio,FronteraNueva).



% Actualiza la frontera con un Nodo hijo (o vecino) del Nodo padre en el
% formato de notacion de frontera nodo(ID,CostoCamino,Camino,Heuristica)
% Caso base:
actualizarFrontera(nodo(IDNodoPadre,CostoCaminoPadre,CaminoPadre,_HeuristicaPadre) %Nodo padre o actual
                  ,IDNodoHijo %ID Nodo hijo o vecino
                  ,CostoPaso %Costo del paso del Nodo padre a Nodo hijo g()
                  ,[] %Frontera vieja vacia (caso base)
                  ,[nodo(IDNodoHijo,CostoCamino,CaminoNuevo,Heuristica)] %Frontera nueva con el Nodo hijo
                  ):-

    CostoCamino is CostoCaminoPadre + CostoPaso, %Nuevo costo

    append([IDNodoPadre],CaminoPadre,CaminoNuevo), %Nuevo camino
    %writeln('AF| Caso base frontera vacia'),

    calcularHeuristica(IDNodoHijo,Heuristica). %Heuristica de Nodo hijo h()


% Actualiza la frontera con un Nodo hijo (o vecino) del Nodo padre en el
% formato de notacion de frontera nodo(ID,CostoCamino,Camino,Heuristica)
% Caso recursivo:
actualizarFrontera(nodo(IDNodoPadre,CostoCaminoViejo,CaminoViejo,_HeuristicaVieja) %Nodo padre o actual
                  ,IDNodoHijo %IDNodo hijo o vecino
                  ,CostoPaso %Costo paso del Nodo padre al Nodo hijo g()
                  ,FronteraVieja %Frontera vieja
                  ,FronteraNueva %Frontera nueva con Nodo hijo
                  ):-

    CostoCamino is CostoCaminoViejo + CostoPaso,

    append([IDNodoPadre],CaminoViejo,CaminoNuevo),

    calcularHeuristica(IDNodoHijo,Heuristica),

    %writeln('AF| Caso recursivo voy a agregar el primer vecino'),

    insertarSinRepetir(nodo(IDNodoHijo,CostoCamino,CaminoNuevo,Heuristica),FronteraVieja,FronteraNueva).

% Convierte una lista de IDs de Nodos a una lista de acciones
% move(IDNodo)
convertirAccion([],[]).
convertirAccion([Nodo|RestoCamino],[move(Nodo)|RestoPlan]):-
    convertirAccion(RestoCamino,RestoPlan).

% Invierte una lista de IDs de Nodos, Obtiene el destino final y
% convierte la lista en una lista de acciones move(IDNodo)
invertirYCrearPlan([Destino|RestoCamino],Plan,Destino):-
    reverse([Destino|RestoCamino],CaminoOrdenado),
    convertirAccion(CaminoOrdenado,Plan).


% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
% Llama a busquedaAEstrella con las metas, el nodo actual, los vecinos
% del nodo actual, y al nodo actual como visitado
buscar_plan_desplazamiento(Metas, RestoPlan, Destino):-
    at([agent,me],IDNodoActual),
    %write('BPD| Estoy en el nodo: '),write(IDNodoActual),nl,

    generarVecinos([],IDNodoActual,Vecinos),
    %writeln('BPD| Genere vecinos'),

    obtenerFrontera(nodo(IDNodoActual,0,[],0),Vecinos,[],FronteraOrdenada),
    %writeln('BPD| Obtuve frontera ordenada:'),
    %escribirFrontera(FronteraOrdenada),

    %La frontera ya la ordena "obtenerFrontera"
    %ordenar_por_f(FronteraDesordenada,FronteraOrdenada),

    %writeln('BPD| LLAMO POR PRIMERA VEZ A BUSQUEDA A*'),
    !,busquedaAEstrella(Metas,nodo(IDNodoActual,0,[],0),FronteraOrdenada,[IDNodoActual],CaminoAMeta),

    invertirYCrearPlan(CaminoAMeta,[_PrimerMove|RestoPlan],Destino).

% Algoritmo A* Caso base:
%
% +Metas: Lista nodos meta
% +NodoActual: Nodo actual
% +Frontera: Lista de nodos en la frontera
% +Visitados: Lista de nodos visitados
% -Plan: Camino de costo minimo hacia una meta
busquedaAEstrella(Metas,nodo(IDNodoActual,_Cc,PlanAnterior,_H),_Frontera,_Visitados,Plan):-

    member(IDNodoActual,Metas),

    append([IDNodoActual],PlanAnterior,Plan).

% Algoritmo A* Caso recursivo:
%
% +Metas: Lista nodos meta
% +NodoActual: Nodo actual
% +Frontera: Lista de nodos en la frontera.
% +Visitados: Lista de nodos visitados
% -Plan: Camino de costo minimo hacia una meta
busquedaAEstrella(Metas,_NodoActual,Frontera,Visitados,Plan):-

    seleccionar(nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),Frontera,FronteraReducida),
    %writeln('A*| Seleccione primero'),

    agregarVisitado(IDNodoElegido,Visitados,VisitadosConNodo),
    %writeln('A*| Agregue visitado'),

    generarVecinos(Visitados,IDNodoElegido,Vecinos),
    %writeln('A*| Genere vecinos'),

    obtenerFrontera(nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),Vecinos,FronteraReducida,FronteraOrdenada),
    %writeln('A*| Obtuve frontera ordenada:'),
    %escribirFrontera(FronteraOrdenada),

    %La frontera ya la ordena "obtenerFrontera"
    %ordenar_por_f(FronteraConVecinos,FronteraOrdenada),

    busquedaAEstrella(Metas,nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),FronteraOrdenada,VisitadosConNodo,Plan).


% Inserta ORDENADAMENTE en una lista (menor a mayor)
% El formato de los elementos depende del problema (hay que cambiarlos)
insertar(Elem,[],[Elem]).
insertar(nodo(ID1,CostoCamino1,Camino1,Heuristica1),
         [nodo(ID2,CostoCamino2,Camino2,Heuristica2)|Resto],
         [nodo(ID1,CostoCamino1,Camino1,Heuristica1),nodo(ID2,CostoCamino2,Camino2,Heuristica2)|Resto]):-

    Val1 is CostoCamino1 + Heuristica1,
    Val2 is CostoCamino2 + Heuristica2,
    Val1 =< Val2,!.
insertar(Elem,[Cabeza|Resto],[Cabeza|ListaOrd]):-
    insertar(Elem,Resto,ListaOrd).



% Inserta si no existe nodo con mismo id
%
insertarSinRepetir(nodo(ID,CostoCamino1,Camino1,Heuristica1),Lista,ListaNueva):-

    not(member(nodo(ID,_CostoCamino2,_Camino2,_Heuristica2),Lista)),
    %writeln('ISR| No hay un nodo con el mismo id, inserto'),
    insertar(nodo(ID,CostoCamino1,Camino1,Heuristica1),Lista,ListaNueva),!.
% Existe nodo con mismo id pero el nuevo a agregar es menor
%
insertarSinRepetir(nodo(ID,CostoCamino1,Camino1,Heuristica1),Lista,ListaNueva):-

    member(nodo(ID,CostoCamino2,Camino2,Heuristica2),Lista),
    Val1 is CostoCamino1 + Heuristica1,
    Val2 is CostoCamino2 + Heuristica2,
    Val1 =< Val2,
    %writeln('ISR| Hay un nodo con el mismo id pero el nuevo es mejor, borro e inserto'),
    borrar(nodo(ID,CostoCamino2,Camino2,Heuristica2),Lista,ListaSinNodo),
    insertar(nodo(ID,CostoCamino1,Camino1,Heuristica1),ListaSinNodo,ListaNueva),!.

% La unica otra posibilidad es que exista nodo con mismo id
% pero este es menor al que quiero agregar, no hago nada.
%
insertarSinRepetir(_Elem,Lista,Lista).
   %:-writeln('ISR| Hay un nodo con el mismo id pero el nuevo es peor no hago nada').




%Borra el elemento X de una lista si existe
borrar(_X,[],[]).
borrar(X,[X|Ys],Ys).
borrar(X,[Y|Ys],[Y|Zs]):-
    borrar(X,Ys,Zs).

%Mantiene el minimo elemento en una lista
minimo(Elem,[],Elem).
minimo(Elem,[MinV],MinV):- MinV =< Elem,!.
minimo(Elem,[_MinV],Elem).














