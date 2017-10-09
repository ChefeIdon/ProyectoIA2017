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


seleccionar(Nodo,[Nodo|RestoFrontera],RestoFrontera).


eliminarNodoInicial([_NodoInicial|RestoFrontera],RestoFrontera).


generarVecinos(IDNodo,Vecinos):-
    node(IDNodo,_Pos,Vecinos).


ordenarDistancias(DistanciasDesordenadas,DistanciasOrdenadas):-
    quick_sortNumeros(DistanciasDesordenadas,DistanciasOrdenadas).

obtenerDistancias(_VectorNodo,[],[]).

obtenerDistancias(VectorNodo,[IDNodoMeta|RestoNodosMeta],[Distancia|RestoDistancias]):-
    node(IDNodoMeta,VectorMeta,_Ady),
    distance(VectorNodo,VectorMeta,Distancia),
    obtenerDistancias(VectorNodo,RestoNodosMeta,RestoDistancias).

calcularHeuristica(IDNodo,MenorDistancia):-
    node(IDNodo,VectorNodo,_Ady),
    findall(IDNodoMeta,at([gold,_Nombre],IDNodoMeta),IDNodosMeta),
    obtenerDistancias(VectorNodo,IDNodosMeta,Distancias),
    ordenarDistancias(Distancias,[MenorDistancia|_RestoDistancia]).

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

obtenerFrontera(_NodoActual,[],Frontera,Frontera).
   %write('    OF1| Vecinos vacio | NodoPadre:'),write(NodoActual),nl,
   %write('    OF1| Frontera resultante: '),nl,
   %forall(member(Nodo,Frontera),escribirFrontera(Nodo)),nl.

   %actualizarFrontera(NodoActual,IDNodoAdy,CostoPaso,FronteraVieja,FronteraNueva),nl.


obtenerFrontera(NodoActual,[[IDNodoAdy,CostoPaso]|RestoVecinos],[],FronteraNueva):-
   %write('    OF2| Frontera vieja vacia | NodoPadre:'),write(NodoActual),nl,

   actualizarFrontera(NodoActual,IDNodoAdy,CostoPaso,[NodoActual],FronteraPasoMedio),

   obtenerFrontera(NodoActual,RestoVecinos,FronteraPasoMedio,FronteraNueva).

obtenerFrontera(NodoPadre,[[IDNodoVecino,CostoPaso]|RestoVecinos],FronteraVieja,FronteraNueva):-
   %write('    OF3| Nodo padre:'),write(NodoPadre),write('| IDNodo hijo(vecino):'),write(IDNodoVecino),nl,

   %write('OF3| La frontera vieja es: '),nl,
   %forall(member(Nodo,FronteraVieja),escribirFrontera(Nodo)),nl,

   %write('OF3| Voy a llamar a actualizar frontera para agregar al nodo hijo: '),write(IDNodoVecino),nl,
   actualizarFrontera(NodoPadre,IDNodoVecino,CostoPaso,FronteraVieja,FronteraPasoMedio),

   %write('OF3| Sali de actualizarFrontera con: NodoPadre:'),write(NodoPadre),nl,
   %write('OF3| IDNodo hijo: '),write(IDNodoVecino),nl,

   obtenerFrontera(NodoPadre,RestoVecinos,FronteraPasoMedio,FronteraNueva).




% Por cada vecino del nodo padre los agrega a la frontera si es que ya
% no estan ahi

actualizarFrontera(NodoPadre,NodoHijo,CostoPaso,[],[nodo(NodoHijo,CostoPaso,[NodoPadre],Heuristica)]):-

    %write('        AFb| NodoPadre:'),write(NodoPadre),write('| IDNodoHijo(vecino):'),write(NodoHijo),nl,

    calcularHeuristica(NodoHijo,Heuristica).

actualizarFrontera(nodo(IDNodoPadre,CostoCaminoViejo,CaminoViejo,_HeuristicaVieja),
                   IDNodoHijo,CostoPaso,FronteraVieja,FronteraNueva):-

    %write('        AFr| IDNodoPadre:'),write(IDNodoPadre),write('| IDNodoHijo(vecino):'),write(IDNodoHijo),nl,

    %Si el nodo no esta en la frontera...
    %write('        AFr| ('),write(IDNodoHijo),write(') en frontera?'),nl,
    %noEsMiembro(nodo(IDNodoHijo,_AlgunCostoCamino,_AlgunCamino,_AlgunaH),FronteraVieja),
    delete_if_exists(nodo(IDNodoHijo,_AlgunCostoCamino,_AlgunCamino,_AlgunaH),FronteraVieja,FronteraSinRepetido),


    %write('AFr| La frontera al principio es: '),nl,
    %forall(member(UnNodo,FronteraVieja),escribirFrontera(UnNodo)),nl,

    CostoCamino is CostoCaminoViejo + CostoPaso,

    append([IDNodoPadre],CaminoViejo,CaminoNuevo),

    calcularHeuristica(IDNodoHijo,Heuristica),
    append([nodo(IDNodoHijo,CostoCamino,CaminoNuevo,Heuristica)],FronteraSinRepetido,FronteraNueva).

convertirAccion([],[]).
convertirAccion([Nodo|RestoCamino],[move(Nodo)|RestoPlan]):-
    convertirAccion(RestoCamino,RestoPlan).

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
    %write('BPD| Genere los vecinos del nodo actual: ['),
    %forall(member([IDNodo,_Costo],Vecinos),escribirNodos(IDNodo)),write(']'),nl,


    obtenerFrontera(nodo(IDNodoActual,0,[],0),Vecinos,[],FronteraDesordenada),
    %write('BPD| Obtuve la frontera del nodo inicial:'),write(IDNodoActual),nl,

    ordenar_por_f(FronteraDesordenada,FronteraOrdenada),
    %write('BPD| Ordene esa frontera por F'),nl,

    eliminarNodoInicial(FronteraOrdenada,FronteraOrdenadaSinInicial),
    %write('BPD| Elimine el nodo inicial'),nl,


    %write('BPD| LLAMO POR PRIMERA VEZ A BUSQUEDA A*'),nl,
    %write('BPD| Con frontera inicial: '),nl,
    %forall(member(UnNodo,FronteraOrdenadaSinInicial),escribirFrontera(UnNodo)),nl,

    busquedaAEstrella(Metas,nodo(IDNodoActual,0,[],0),FronteraOrdenadaSinInicial,[IDNodoActual],CaminoAMeta),

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













