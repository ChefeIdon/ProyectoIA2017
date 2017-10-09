:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).

%Esto esta bien?
:- dynamic busquedaAestrella/3.

escribirFrontera(nodo(ID,CostoCamino,Camino,Heuristica)):-
    write('Nodo: '),write(ID),write(' CostoCamino: '),write(CostoCamino),
    write(' Camino: '),write(Camino),write(' Heuristica: '),write(Heuristica),nl.

escribirNodos(IDNodo):-
    write(IDNodo),write(',').

%Selecciona el primer nodo
seleccionar(Nodo,[Nodo|RestoFrontera],RestoFrontera).

%Obtiene los vecinos de un nodo
generarVecinos(IDNodo,Vecinos):-
    node(IDNodo,_Pos,Vecinos).


% Ordena una lista de distancias (que son las heuristicas de un nodo a
% las metas)
obtenerDistancias(_VectorNodo,[],[]).

ordenarDistancias(DistanciasDesordenadas,DistanciasOrdenadas):-
    quick_sortNumeros(DistanciasDesordenadas,DistanciasOrdenadas).

% Obtiene las distancias de un nodo a todos los nodos que tienen un
% tesoro (metas) que son las heuristicas
obtenerDistancias(VectorNodo,[IDNodoMeta|RestoNodosMeta],[Distancia|RestoDistancias]):-
    node(IDNodoMeta,VectorMeta,_Ady),
    distance(VectorNodo,VectorMeta,Distancia),
    obtenerDistancias(VectorNodo,RestoNodosMeta,RestoDistancias).

%Obtiene la mejor
calcularHeuristica(IDNodo,MenorDistancia):-
    node(IDNodo,VectorNodo,_Ady),
    findall(IDNodoMeta,at([gold,_Nombre],IDNodoMeta),IDNodosMeta),
    obtenerDistancias(VectorNodo,IDNodosMeta,Distancias),
    ordenarDistancias(Distancias,[MenorDistancia|_RestoDistancia]).

%Ordena una cantidad
ordenar_por_f(FronteraDesordenada,FronteraOrdenada):-
    quick_sortNodos(FronteraDesordenada,FronteraOrdenada).



noEsMiembro(Elemento,[Elemento|_]):-
    !,fail.

noEsMiembro(Elemento,[_|RestoLista]):-
    !,noEsMiembro(Elemento,RestoLista).

noEsMiembro(_,[]).


agregarVisitado([IDNodo,Costo],[],[IDNodo,Costo]).

agregarVisitado(Nodo,Visitados,VisitadosConNodo):-
    append([Nodo],Visitados,VisitadosConNodo).

% Recorre una lista de Nodos vecinos de la forma [IDNodo,CostoPaso] y
% utiliza el predicado "actualizarFrontera" para agregar de a un Nodo
% vecino a la frontera de Nodos de la forma
% nodo(ID,CostoCamino,Camino,Heuristica).
% Utiliza el Nodo padre para calcular el camino y el costo del camino
% del Nodo vecino
obtenerFrontera(_NodoActual %Nodo padre
               ,[] %Lista de vecinos (caso base)
               ,Frontera %Frontera de nodos
               ,Frontera). %Frontera de nodos

obtenerFrontera(NodoPadre %Nodo padre
               ,[[IDNodoVecino,CostoPaso]|RestoVecinos] %Lista vecinos con un vecino y el resto de la lista
               ,[] %Frontera inicial vacia (caso base)
               ,FronteraNueva):- %Frontera final con el primer vecino

   actualizarFrontera(NodoPadre,IDNodoVecino,CostoPaso,[],FronteraPasoMedio),

   obtenerFrontera(NodoPadre,RestoVecinos,FronteraPasoMedio,FronteraNueva).


obtenerFrontera(NodoPadre %Nodo padre
               ,[[IDNodoVecino,CostoPaso]|RestoVecinos] %Lista de vecinos con un vecino y el resto de la lista
               ,FronteraVieja %Frontera inicial
               ,FronteraNueva %Frontera final con el primer vecino
               ):-

   actualizarFrontera(NodoPadre,IDNodoVecino,CostoPaso,FronteraVieja,FronteraPasoMedio),

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

    delete_if_exists(nodo(IDNodoHijo,_AlgunCostoCamino,_AlgunCamino,_AlgunaH),FronteraVieja,FronteraSinRepetido),

    CostoCamino is CostoCaminoViejo + CostoPaso,

    append([IDNodoPadre],CaminoViejo,CaminoNuevo),

    calcularHeuristica(IDNodoHijo,Heuristica),

    append([nodo(IDNodoHijo,CostoCamino,CaminoNuevo,Heuristica)],FronteraSinRepetido,FronteraNueva).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
%
%
%Hacer: Usar un planificador para satisfacer varias metas.
buscar_plan_desplazamiento(Metas, RestoPlan, Destino):-
    %write('Entre a buscar_plan_desplazamiento'),nl,
    write('  BPD| Los nodos metas son:'),write(Metas),nl,

    at([agent,me],IDNodoActual),
    write('  BPD| Estoy en el nodo: '),write(IDNodoActual),nl,

    generarVecinos(IDNodoActual,Vecinos),

    obtenerFrontera(nodo(IDNodoActual,0,[],0),Vecinos,[],FronteraDesordenada),

    ordenar_por_f(FronteraDesordenada,FronteraOrdenada),
    %write('BPD| Ordene esa frontera por F'),nl,

    %write('BPD| LLAMO POR PRIMERA VEZ A BUSQUEDA A*'),nl,
    %write('BPD| Con frontera inicial: '),nl,
    %forall(member(UnNodo,FronteraOrdenadaSinInicial),escribirFrontera(UnNodo)),nl,

    busquedaAEstrella(Metas,nodo(IDNodoActual,0,[],0),FronteraOrdenada,[IDNodoActual],CaminoAMeta),

    %write('BPD| Salgo de Busqueda A* con camino a meta: '),write(CaminoAMeta),nl,

    invertirYCrearPlan(CaminoAMeta,[_PrimerMove|RestoPlan],Destino),

    write('  BPD| El plan es: '),write(RestoPlan),nl,
    write('  BPD| Destino:'),write(Destino),nl.

%    forall(member(Nodo,Camino),append(move(Nodo),Plan)),

 %   write('Ya genere plan y ahora lo recuerdo'),nl,

  %  assert(plan(Plan))



busquedaAEstrella(Metas,nodo(IDNodoActual,_Cc,PlanAnterior,_H),_Frontera,_Visitados,Plan):-
    %write('  A* base'),nl,

    %seleccionar(nodo(IDNodoElegido,_CostoCamino,Plan,_Heuristica),Frontera,_FronteraReducida),
    %write('A*| Seleccione el primer nodo: '),write(IDNodoElegido),nl,
    %write('  A*| Es el nodo actual una meta?'),nl,
    member(IDNodoActual,Metas),
    append([IDNodoActual],PlanAnterior,Plan),
    write('    A*| LLEGUE A UNA META!'),nl.
    %write('  A*| Nodo meta: '),write(IDNodoActual),write(' Metas: '),write(Metas),
    %write(' Camino hacia meta: '),write(Plan),nl.


busquedaAEstrella(Metas,_IDNodoActual,Frontera,Visitados,Plan):-
    %nl,write('A* recursivo... Nodo actual:'),write(IDNodoActual),nl,

    %write('A*r| Frontera al principio es: '),nl,
    %forall(member(Nodo,Frontera),escribirFrontera(Nodo)),nl,

    seleccionar(nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),Frontera,FronteraReducida),
    %write('  A*r| Seleccione el primer nodo: '),write(IDNodoElegido),write(' con costo: '),write(CostoCamino),
    %write(', heuristica: '),write(Heuristica),write(', y el camino es: '),write(Camino),nl,

    agregarVisitado(IDNodoElegido,Visitados,VisitadosConNodo),
    %write('A*r| Agregue como visitado al nodo: '),write(IDNodoElegido),write(' Nodos visitados: '),write(VisitadosConNodo),nl,

    %write('A*| Voy a generar la diferencia de vecinos/3'),nl,
    generarVecinos(IDNodoElegido,Vecinos),
    %write('A*r| Genere vecinos del nodo elegido: '),write(Vecinos),nl,

    obtenerFrontera(nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),Vecinos,FronteraReducida,FronteraConVecinos),
    %write('  A*r| Agregue los vecinos del nodo elegido a la frontera... '),write(FronteraConVecinos),nl,

    ordenar_por_f(FronteraConVecinos,FronteraOrdenada),
    %write('  A*r| Ordene la frontera con vecinos: '),nl,
    %forall(member(Nodo,FronteraOrdenada),escribirFrontera(Nodo)),nl,

    %write('  A*r| Llamo recursivamente...'),nl,
    busquedaAEstrella(Metas,nodo(IDNodoElegido,CostoCamino,Camino,Heuristica),FronteraOrdenada,VisitadosConNodo,Plan).


quick_sortNodos(Lista,ListaOrdenada):-
    q_sortNodos(Lista,[],ListaOrdenada).

q_sortNodos([],Acc,Acc).

q_sortNodos([Cabeza|Cola],Acc,ListaOrdenada):-
	pivotingNodos(Cabeza,Cola,Lista1,Lista2),
	q_sortNodos(Lista1,Acc,Lista1Ordenada),
        q_sortNodos(Lista2,[Cabeza|Lista1Ordenada],ListaOrdenada).


pivotingNodos(_Cabeza,[],[],[]).

pivotingNodos(nodo(_Id,CostoCaminoCabeza,_Camino,HeuristicaCabeza),
         [nodo(_IdNodo,CostoCaminoNodo,_CaminoNodo,HeuristicaNodo)|T],
         [nodo(_IdNodo,CostoCaminoNodo,_CaminoNodo,HeuristicaNodo)|L],
         G):-

    CostoNodo is CostoCaminoNodo + HeuristicaNodo,
    CostoCabeza is CostoCaminoCabeza + HeuristicaCabeza,

    CostoNodo>=CostoCabeza,

    pivotingNodos(nodo(_Id,CostoCaminoCabeza,_Camino,HeuristicaCabeza),T,L,G).

pivotingNodos(nodo(_Id,CostoCaminoCabeza,_Camino,HeuristicaCabeza),
         [nodo(_IdNodo,CostoCaminoNodo,_CaminoNodo,HeuristicaNodo)|T],
         L,
         [nodo(_IdNodo,CostoCaminoNodo,_CaminoNodo,HeuristicaNodo)|G]
        ):-

    CostoNodo is CostoCaminoNodo + HeuristicaNodo,
    CostoCabeza is CostoCaminoCabeza + HeuristicaCabeza,
    CostoNodo<CostoCabeza,

    pivotingNodos(nodo(_Id,CostoCaminoCabeza,_Camino,HeuristicaCabeza),T,L,G).


quick_sortNumeros(List,Sorted):-
    q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).

pivoting(_H,[],[],[]).
pivoting(H,[X|T],[X|L],G):-
    X>=H,
    pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-
    X<H,
    pivoting(H,T,L,G).













