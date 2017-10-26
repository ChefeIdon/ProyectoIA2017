
:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2,
	    atReciente/1
	  ]).

:-[extras_for_agents].

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2, atReciente/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 7
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado din�mico (creencia)
% manipulado por la actualizaci�n de creencias, para que puedan ser
% consultadon por el resto del c�digo del agente.
%

update_beliefs(Perc):-


	%Imprimio las creencias

	%imprimirCreencias,

	%Agrego todas las percepciones nuevas restantes
	forall((member(Rel, Perc), noPercibido(Rel)), assert(Rel)),
	%nl,
	%writeln('Agrego las nuevas percepciones')

	%Elimino mis creencias anteriores
	retractall(atReciente(_)),

	%Actualizar creencias Time
	retractall(time(_)),
        member(time(Time), Perc),
        assert(time(Time)),
	nl,
	%writeln('Actualizo el tiempo:'),


	%Actualizar todas las entidades que formaban parte de un has y ahora forman parte de un at
	actualizarHasAt(Perc),
	%nl,
	%writeln('Actualizo una entidad que contenia un agente y ahora esta en el suelo.'),


	%Actualizar todas las entidades que formaban parte de un at y ahora forman parte de un has
	actualizarAtHas(Perc),
	%nl,
	%writeln('Actualizo las entidades que estaban en el suelo y ahora las posee un agente.'),

	%Actualizar Posicion de una Entidad
	actualizarPosEntidad(Perc),
	%nl,
	%writeln('Actualizo las posiciones de las entidades'),

	%Actualizar la tenencia de un objeto que tenia un agente viejo y ahora lo tiene un agente nuevo
	actualizarTenenciasAgentes(Perc),
	%nl,
	%writeln('Actualizo la tenencia de una entidad entre agentes'),

	%Actualizar las tenencias de mi propio agente.
	actualizarMisTenencias(Perc),
	%nl,

	%Actualizar las propiedades de las entidades
	actualizarPropEntidades(Perc),
	%nl,
	%writeln('Actualizar las propiedades de los agentes'),

	%Actualizar posiciones entidades en rango de vision
	actualizarEntidadesEnRango(Perc).
	%nl.
	%writeln('Actualizo las posiciones de las entidades en un determinado rango').



actualizarHasAt(Perc):-
	forall((member(at(Ent1,_PosEnt1),Perc),has(Agent1,Ent1)) , (retract(has(Agent1,Ent1)))),!.

actualizarHasAt(_Perc):-
	true.




actualizarAtHas(Perc):-
	forall((member(has(_Ent7,Ent6),Perc), at(Ent6,PosEnt6), atPos(Ent6,VectorPos6)) , (retract(at(Ent6,PosEnt6)), retract(atPos(Ent6,VectorPos6)))),!.

actualizarAtHas(_Perc):-
	true.


actualizarPosEntidad(Perc):-
	forall((member(at(Ent2,PosEnt2),Perc),at(Ent2,PosEnt3),atPos(Ent2,VectorPos3),PosEnt2 \= PosEnt3), (retract(at(Ent2,PosEnt3)), retract(atPos(Ent2,VectorPos3)), assert(atReciente(Ent2)))),!.

actualizarPosEntidad(_Perc):-
	true.




actualizarTenenciasAgentes(Perc):-
	forall((member(has(Ent3,Ent4),Perc), has(Ent5,Ent4), Ent5 \= Ent3), (retract(has(Ent5,Ent4)))),!.

actualizarTenenciasAgentes(_Perc):-
	true.



actualizarMisTenencias(Perc):-
	forall(((has([agent,me],Ent4)) , not(member(has([agent,me],Ent4),Perc))), (retract(has([agent,me],Ent4)))),!.
actualizarMisTenencias(_Perc):-
	true.


actualizarPropEntidades(Perc):-
	forall((member(entity_descr(Ent8,Props1),Perc), entity_descr(Ent8,Props2)),(retract(entity_descr(Ent8,Props2)),assert(entity_descr(Ent8,Props1)))),!.

actualizarPropEntidades(_Perc):-
	true.




actualizarEntidadesEnRango(Perc):-
	at([agent,me],MiNodo),
	node(MiNodo,VNodo,_Ady),
	findall(Ent9,(atPos(Ent9,VectorPos9), distance(VNodo,VectorPos9,Dist), Dist<10, not(member(atPos(Ent9,VectorPos9),Perc))),ListaABorrar),
	forall((member(atPos(EntB,_vector),ListaABorrar)),(retract(atPos(EntB,_vector)),retract(at(EntB,_nodo)))),!.

actualizarEntidadesEnRango(_Perc):-
	true.


%Si encuentro un at(E1,_), que ya tengo, estonces no lo agrego
noPercibido(at(E1,P1)):-
        forall((at(E2,P2),E1 = E2, P1 = P2), false).

noPercibido(atPos(E1,P1)):-
        forall((atPos(E2,P2),E1 = E2, P1 = P2), false).

%noPercibido(at(E1,P1)):-
	%forall((at(E2,P2)),(E1 \= E2)).
        %forall((at(E2,P2),E1=E2), )
	 %      %; (P1 \= P2))

%noPercibido(atPos(E1,P1)):-
	%forall((atPos(E2,P2)),(E1 \= E2)).

noPercibido(has(E1,P1)):-
	forall((has(E1,P2)),(P1 \= P2)).

noPercibido(entity_descr(E1,P1)):-
	forall((entity_descr(E2,_P2)), (P1 \= [], E1 \= E2)).

noPercibido(node(Nodo1,_Vector1,_Ady1)):-
        forall((node(Nodo2,_Vector2,_Ady2)), (Nodo1 \= Nodo2)).



imprimirCreencias:-
	nl,
	forall((has([agent,me],Ent1)), (write('La entidad que tengo es: '),writeln(Ent1))),
	forall((has([agent,NomAgent],Ent2), NomAgent \= me),
	       (write('El agente'),write(NomAgent), write(' tiene la entidad: '),writeln(Ent2))),
	forall((at([gold,IDOro],_Pos1)), (write('El oro que esta en el suelo es: '),writeln(IDOro))),
	forall((at([potion,IDPot],_Pos2)), (write('La pocion que esta en el suelo es: '),writeln(IDPot))).



































