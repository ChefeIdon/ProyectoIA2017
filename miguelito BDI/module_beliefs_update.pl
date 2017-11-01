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
	
	/*
		el agente olvida todo lo que recordaba
	*/
	write('Olvido'),nl,
	retractall(time(_)),
	forall(member(Relacion,Percepcion),olvidar(Relacion)),
	
	write('Recuerdo'),nl,
	/*
		el agente recuerda todo lo percibido
	*/
	forall(member(Relacion,Percepcion),recordar(Relacion)),
	write(Percepcion).

olvidar(at(Entidad,_)) :-			% <- Si percibe una entidad en el piso
	has(_,Entidad),					% <- que estaba en posesión de otra entidad
	retractall(has(_,Entidad)).		% <- Se olvida de esa posesión.

olvidar(at(Entidad,_)) :-			% <- Si percibe una entidad en el piso
	at(Entidad,_),					% <- que estaba en otro lugar del piso
	retractall(at(Entidad,_)),		% <- Se olvida de la ubicación anterior.
	retractall(atPos(Entidad,_)).	% <- Se olvida de la posición (vector) anterior.

olvidar(atPos(Entidad,_)) :-		% <- Si percibe una entidad en una posición (vector)
	atPos(Entidad,_),				% <- que estaba en otra posición (vector)
	retractall(at(Entidad,_)),		% <- Se olvida de la ubicación anterior.
	retractall(atPos(Entidad,_)).	% <- Se olvida de la posición (vector) anterior.
	
olvidar(atPos(Entidad,_)) :-		% <- Si percibe una entidad en el piso
	has(_,Entidad),					% <- que estaba en posesión de otra entidad
	retractall(has(_,Entidad)).		% <- Se olvida de esa posesión.
	
olvidar(has(_,Entidad)) :-			% <- Si percibe que una entidad es poseida por otra entidad
	at(Entidad,_), 					% <- que creia que estaba en el piso
	atPos(Entidad,_),				% <- que estaba en otra posición (vector)
	retractall(at(Entidad,_)), 		% <- Olvida que estaba en el piso
	retractall(atPos(Entidad,_)).	% <- Olvida su posición (vector) anterior

olvidar(has(Entidad,_)) :-			% <- Si percibe una entidad
	has(Entidad,_), 				% <- que ya habia visto antes
	retractall(has(Entidad,_)).		% <- la olvida.

%olvidar(node(Id,_,_)) :-			% <- Si percibe un nodo
%	node(Id,_,_),					% <- que ya habia visto antes
%	retractall(node(Id,_,_)).		% <- olvida el nodo visto.

%olvidar(node(Id,Pos,_)) :-			% <- Si percibe un nodo
%	at(_,Id),						% <- que ya habia visto algo en su ubicación
%	atPos(_,Pos),					% <- y que ya habia visto algo en su posición (vector)
%	retractall(at(_,Id)),			% <- Olvido lo que habia visto en esa ubicación
%	retractall(atPos(_,Pos)).		% <- Olvido lo que habia visto en esa posición (vector)

olvidar(entity_descr(Entidad,_)) :-			% <- Si percibe la descripcion de una entidad
	entity_descr(Entidad,_),				% <- que ya la habia recordado antes
	retractall((entity_descr(Entidad,_))).	% <- Olvido la descripción anterior

olvidar(X).					% <- Si percibe algo nuevo no hace nada

recordar(X):-				% <- Si percibe algo  
	X,!.					% <- que ya recuerda no hace nada.

recordar(X) :- assert(X). 	% <- Si percibe algo nuevo lo recuerda.
	
	
	
	
	
	