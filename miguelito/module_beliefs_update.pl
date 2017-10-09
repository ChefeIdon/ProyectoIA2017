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
% IMPORTANTE: Debe exportarse todo predicado din�mico (creencia)
% manipulado por la actualizaci�n de creencias, para que puedan ser
% consultadon por el resto del c�digo del agente.
%

update_beliefs(Percepcion):-
	forall(member(Relacion,Percepcion),actualizar(Relacion)).

actualizar(atPos(Entidad,PosVector)):-
	retractall(atPos(Entidad,_PosVectorViejo)),
	assert(atPos(Entidad,PosVector)).

actualizar(entity_descr(Entidad,Descripcion)):-
	retractall(entity_descr(Entidad,_DescripcionVieja)),
	assert(entity_descr(Entidad,Descripcion)).

actualizar(has(EntidadDuena,Entidad)):-
	at(Entidad,_PosVieja), %Recordamos que esa entidad estaba en alguna posicion?
	retractall(at(Entidad,_PosVieja)), %Borrar donde estaba
	assert(has(EntidadDuena,Entidad)). %Recordar quien la tiene

actualizar(has(EntidadDuena,Entidad)):-
	retractall(has(EntidadDuena,_EntidadViejas)), %Borrar las entidades viejas que tenia
	assert(has(EntidadDuena,Entidad)). %Recordar nueva entidad que posee

actualizar(time(T)):-
	retractall(time(_)), %Borrar tiempo anterior
	assert(time(T)). %Recordar tiempo actual

actualizar(node(Id,Pos,Ad)):-
	retractall(node(Id,_Pos,_Ad)), %Borrar previa conocimiento del nuevo nodo
	assert(node(Id,Pos,Ad)). %Recordar nodo


actualizar(at(Entidad,PosNueva)):-
	has(_AlgunaEntidad,Entidad), %Recordamos que esa entidad la tenia otro entidad?
	retractall(has(_AlgunaEntidad,Entidad)), %Borrar quien la tenia
	assert(at(Entidad,PosNueva)). %Recordar nueva ubicacion


actualizar(at(Entidad,PosNueva)):-
	retractall(at(Entidad,_PosVieja)), %Borrar ubicacion vieja
	assert(at(Entidad,PosNueva)). %Recordar nueva ubicacion




























