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
% IMPORTANTE: Debe exportarse todo predicado din谩mico (creencia)
% manipulado por la actualizaci贸n de creencias, para que puedan ser
% consultadon por el resto del c贸digo del agente.
%

update_beliefs(Percepcion):-

	/*
		el agente olvida todo lo que recordaba y que ahora hay conflicto
	*/
	%writeln('Olvido'),
	retractall(time(_)),
	forall(member(Relacion,Percepcion),olvidar(Relacion)),

	%writeln('Recuerdo'),
	/*
		el agente recuerda todo lo percibido
	*/
	forall(member(Relacion,Percepcion),recordar(Relacion)),

	%writeln('Checkeo'),
	/*
	      el agente ahora checkea que lo que recordaba sigue siendo correcto
	      Casos:
	      - si se recordaba a un objeto(tesoro o pocion) en una posicion y ahora no esta mas
	      - si se recordaba a una entidad que tenia a otra entidad y ahora no la tiene mas
	*/
	forall(member(node(ID,Pos,Ady),Percepcion), checkearNodos(node(ID,Pos,Ady),Percepcion)),
	forall(member(has(Duenio,_Objeto),Percepcion), checkearPosesion(Duenio,Percepcion)).



checkearNodos(node(IDNodo,_Pos,_Ady), Percepcion):-
	% Para todas las entidades que recordaba en ese nodo (que veo ahora), checkeo si sigue estando ahi
	forall(at(Entidad, IDNodo),checkearAt(Entidad,IDNodo,Percepcion)),
	!.
checkearNodos(_Nodo, _Percepcion).



checkearPosesion(Duenio, Percepcion):-
	% Para todas las entidades (que veo ahora) que recordaban que tenian algun objeto, checkeo si los siguen teniendo
	forall(has(Duenio, Objeto),checkearHas(Duenio,Objeto,Percepcion)),
	!.
checkearPosesion(_Duenio, _Percepcion).




% checkeo si una entidad sigue tienendo a otra entidad como lo recordaba
checkearHas(Duenio, Objeto, Percepcion):-
	%write('Estoy viendo a '),write(Due駉),write(' con '),write(Objeto),write(' en su poder?'),
	member(has(Duenio,Objeto), Percepcion),
	%writeln(' SI!'),
	!.
checkearHas(Duenio, Objeto, _Percepcion):-
	retract(has(Duenio,Objeto)).
	%write(' Ya no veo a '),write(Due駉),write(' con '),write(Objeto),writeln(' en su poder.').


% checkeo si una entidad sigue estando en una posicion como lo recordaba
checkearAt(Entidad, IDNodo, Percepcion):-
	%write('Estoy viendo a '),write(Entidad),write(' en '),write(IDNodo),write('?'),
	member(at(Entidad,IDNodo), Percepcion),
	%writeln(' SI!'),
	!.
checkearAt(Entidad, IDNodo, _Percepcion):-
	retract(at(Entidad,IDNodo)),
	retract(atPos(Entidad,_)).
	%write(' Ya no veo a '),write(Entidad),write(' en '),writeln(IDNodo).

%------------------------------

olvidar(at(Entidad,_)) :-			% <- Si percibe una entidad en el piso
	has(_,Entidad),				% <- que estaba en posesi贸n de otra entidad
	retractall(has(_,Entidad)).		% <- Se olvida de esa posesi贸n.

olvidar(at(Entidad,_)) :-			% <- Si percibe una entidad en el piso
	at(Entidad,_),				% <- que estaba en otro lugar del piso
	retractall(at(Entidad,_)),		% <- Se olvida de la ubicacion anterior.
	retractall(atPos(Entidad,_)).	        % <- Se olvida de la posicion (vector) anterior.

olvidar(atPos(Entidad,_)) :-		% <- Si percibe una entidad en una posicion (vector)
	atPos(Entidad,_),		% <- que estaba en otra posicion (vector)
	retractall(at(Entidad,_)),	% <- Se olvida de la ubicacion anterior.
	retractall(atPos(Entidad,_)).	% <- Se olvida de la posicion (vector) anterior.

olvidar(atPos(Entidad,_)) :-		% <- Si percibe una entidad en el piso
	has(_,Entidad),			% <- que estaba en posesi贸n de otra entidad
	retractall(has(_,Entidad)).	% <- Se olvida de esa posesi贸n.

olvidar(has(_,Entidad)) :-		% <- Si percibe que una entidad es poseida por otra entidad
	at(Entidad,_),			% <- que creia que estaba en el piso
	atPos(Entidad,_),		% <- que estaba en otra posici贸n (vector)
	retractall(at(Entidad,_)),	% <- Olvida que estaba en el piso
	retractall(atPos(Entidad,_)).	% <- Olvida su posici贸n (vector) anterior

olvidar(has(DuenioNuevo,Entidad)) :-		% <- Si percibe una entidad con objeto
	has(DuenioViejo,Entidad),
	DuenioNuevo \= DuenioViejo,
	                                         % <- que ya recordaba que tenia otra entidad
	retractall(has(DuenioViejo,Entidad)).	% <- olvida al duenio viejo

%olvidar(node(Id,_,_)) :-		% <- Si percibe un nodo
%	node(Id,_,_),			% <- que ya habia visto antes
%	retractall(node(Id,_,_)).	% <- olvida el nodo visto.

%olvidar(node(Id,Pos,_)) :-		% <- Si percibe un nodo
%	at(_,Id),			% <- que ya habia visto algo en su ubicaci贸n
%	atPos(_,Pos),			% <- y que ya habia visto algo en su posici贸n (vector)
%	retractall(at(_,Id)),  % <- Olvido lo que habia visto en esa
%	ubicaci贸n retractall(atPos(_,Pos)). % <- Olvido lo que habia
%	visto en esa posici贸n (vector)

olvidar(entity_descr(Entidad,_)) :-		% <- Si percibe la descripcion de una entidad
	entity_descr(Entidad,_),		% <- que ya la habia recordado antes
	retractall((entity_descr(Entidad,_))).	% <- Olvido la descripci贸n anterior

olvidar(_X).					% <- Si percibe algo nuevo no hace nada

recordar(X):-				% <- Si percibe algo
	X,!.				% <- que ya recuerda no hace nada.

recordar(X) :- assert(X).	% <- Si percibe algo nuevo lo recuerda.






