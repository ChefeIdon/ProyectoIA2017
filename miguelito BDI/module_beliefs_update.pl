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
	%write('========================================================================='),nl,

	forall(member(Relacion,Percepcion),actualizar(Relacion)),

	%write('UB| Voy a checkear si las entidades siguen en los nodos en que los vi'),nl,

	forall(member(node(ID,Pos,Ady),Percepcion),checkear(node(ID,Pos,Ady),Percepcion)).


checkear(node(IDNodo,_Pos,_Ady), Percepcion):-
	forall(at(Entidad, IDNodo),checkearEntidad(Entidad,IDNodo,Percepcion)),
	!.
checkear(_Nodo, _Percepcion).

checkearEntidad(Entidad, IDNodo, Percepcion):-
	%write('Estoy viendo a '),write(Entidad),write(' en '),write(IDNodo),write('?'),
	member(at(Entidad,IDNodo), Percepcion),
	%write(' SI!'),nl,
	!.

checkearEntidad(Entidad, IDNodo, _Percepcion):-
	retract(at(Entidad,IDNodo))
	%write('Ya no veo a '),write(Entidad),write(' en '),write(IDNodo),nl
	.

actualizar(atPos(Entidad,Pos)):-
	retractall(atPos(Entidad,_PosVectorViejo)),
	assert(atPos(Entidad,Pos)).
	%write(Entidad),write(' ahora esta en '),write(Pos),nl.

actualizar(entity_descr(Entidad,Descripcion)):-
	retractall(entity_descr(Entidad,_DescripcionVieja)),
	assert(entity_descr(Entidad,Descripcion)).
	%,write(Entidad),write(' entityDesc '),write(Descripcion),nl.

actualizar(has(EntidadDuena,Entidad)):-
	at(Entidad,_PosVieja), %Recordamos que esa entidad estaba en alguna posicion?
	retractall(at(Entidad,_PosVieja)), %Borrar donde estaba
	assert(has(EntidadDuena,Entidad)).
	%,write(Entidad),write(' estaba en el piso y ahora la tiene '),write(EntidadDuena),nl. %Recordar quien la tiene

actualizar(has([agent,IDAgent],[gold,IDGold])):-
	not(has([agent,IDAgent],[gold,IDGold])),
	assert(has([agent,IDAgent],[gold,IDGold])).
	%,write(IDAgent),write(' ahora tiene a '),write(IDGold),nl. %Recordar que el agente posee un tesoro nuevo

actualizar(has(EntidadDuena,Entidad)):-
	not(has(EntidadDuena,Entidad)), %Borrar las entidades viejas que tenia
	assert(has(EntidadDuena,Entidad)).
	%,write(Entidad),write(' la tenia otra entidad y ahora la tiene '),write(EntidadDuena),nl. %Recordar nueva entidad que posee

actualizar(has(EntidadDuena,Entidad)):-
	retractall(has(EntidadDuena,_EntidadViejas)), %Borrar las entidades viejas que tenia
	assert(has(EntidadDuena,Entidad)).
	%,write(Entidad),write(' la sigue teniendo '),write(EntidadDuena),nl. %Recordar nueva entidad que posee

actualizar(time(T)):-
	retractall(time(_)), %Borrar tiempo anterior
	assert(time(T)). %Recordar tiempo actual
	%,write('Tiempo '),write(T),nl

actualizar(node(Id,Pos,Ad)):-
%	write('Nodo: '),write(Id),nl,
	retractall(node(Id,_Pos,_Ad)), %Borrar previa conocimiento del nuevo nodo
	assert(node(Id,Pos,Ad)). %Recordar nodo


actualizar(at(Entidad,PosNueva)):-
	has(_AlgunaEntidad,Entidad), %Recordamos que esa entidad la tenia otro entidad?
	retractall(has(_AlgunaEntidad,Entidad)), %Borrar quien la tenia
	assert(at(Entidad,PosNueva)).
	%write(Entidad),write(' la tenia alguien y la encontre en el piso en '),write(PosNueva),nl. %Recordar nueva ubicacion

actualizar(at(Entidad,Pos)):-
	not(at(Entidad,Pos)),
	assert(at(Entidad,Pos)).
	%,write(Entidad),write(' estaba en otra pos, y se movio a '),write(Pos),nl. %Recordar nueva ubicacion

actualizar(at(Entidad,Pos)):-
	retractall(at(Entidad,_PosVieja)), %Borrar ubicacion vieja
	assert(at(Entidad,Pos)).
	%,write(Entidad),write(' sigue estando en '),write(Pos),nl.




























