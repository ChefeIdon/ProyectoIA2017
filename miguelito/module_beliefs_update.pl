:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultadon por el resto del código del agente.
%

update_beliefs(Percepcion):-
	forall(member(Relacion,Percepcion),actualizar(Relacion)).

/*
	Actualiza la posición de una entidad encontrada
*/
actualizar(atPos(Entidad,PosVector)):-
	retractall(atPos(Entidad,_PosVectorViejo)),
	assert(atPos(Entidad,PosVector)).

/*
	Actualiza la descripción de una entidad encontrada
*/
actualizar(entity_descr(Entidad,Descripcion)):-
	retractall(entity_descr(Entidad,_DescripcionVieja)),
	assert(entity_descr(Entidad,Descripcion)).

/*
	Actualiza la relación has de una entidad encontrada que tiene otra entidad
*/
actualizar(has(EntidadDuena,Entidad)):-
	at(Entidad,_PosVieja),				%Recordamos que esa entidad estaba en alguna posicion?
	retractall(at(Entidad,_PosVieja)),	%Borrar donde estaba
	assert(has(EntidadDuena,Entidad)). 	%Recordar quien la tiene

/*
	Recuerdo la ultima moneda que agarre
*/
actualizar(has([agent,IDAgent],[gold,IDGold])):-
	not(has([agent,IDAgent],[gold,IDGold])),	%si no lo recorde antes
	assert(has([agent,IDAgent],[gold,IDGold])). %Recordar que el agente posee un tesoro nuevo

/*
	Si una entidad tiene a otra, borro si creia que a esta ultima la tenia orta entidad.
*/
actualizar(has(EntidadDuena,Entidad)):-
	retractall(has(EntidadDuena,_EntidadViejas)),	%Borrar las entidades viejas que tenia
	assert(has(EntidadDuena,Entidad)). 				%Recordar nueva entidad que posee

/*
	Actualiza el tiempo
*/
actualizar(time(T)):-
	retractall(time(_)), 	%Borrar tiempo anterior
	assert(time(T)). 		%Recordar tiempo actual

/*
	Actualiza los nodos
*/
actualizar(node(Id,Pos,Ad)):-
	retractall(node(Id,_Pos,_Ad)),	%Borrar previa conocimiento del nuevo nodo
	assert(node(Id,Pos,Ad)). 		%Recordar nodo

/*
	Encuentro una entidad tirada en el piso y me fijo si creia que la tenia alguien mas
*/
actualizar(at(Entidad,PosNueva)):-
	has(_AlgunaEntidad,Entidad), 				%Recordamos que esa entidad la tenia otro entidad?
	retractall(has(_AlgunaEntidad,Entidad)),	%Borrar quien la tenia
	assert(at(Entidad,PosNueva)). 				%Recordar nueva ubicacion

/*
	Encuentro una entidad y actualizo el at
*/
actualizar(at(Entidad,PosNueva)):-
	retractall(at(Entidad,_PosVieja)), 	%Borrar ubicacion vieja
	assert(at(Entidad,PosNueva)).		%Recordar nueva ubicacion




























