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
		El agente comienza olvidando todo lo recordado que es modificado en la nueva percepcion
	*/
	
	%Si percibe un nodo que ya habia visto antes
	%olvida el nodo visto.
	forall((member(node(IdNodo,_VectNuevo,_Ady),Percepcion),node(IdNodo,_VectViejo,_Ady)),retractall(node(IdNodo,_VectViejo,_Ady))),
	
	%Si percibe un nodo que ya habia visto algo en su ubicación y que ya habia visto algo en su posición (vector)
	%Olvida lo que habia visto en esa ubicación y lo que habia visto en esa posición (vector).
	forall((member(node(IdNodo,VectNuevo,_Ady),Percepcion),at(_EntVieja,IdNodo),atPos(_EntVieja,VectNuevo)),(retractall(at(_EntVieja,IdNodo)),retractall(atPos(_EntVieja,VectNuevo)))),
	
	%Si percibe la descripcion de una entidad que ya la habia recordado antes
	%Olvida la descripción anterior.
	forall((member(entity_descr(Entidad,_PosNueva),Percepcion),entity_descr(Entidad,_PosVieja)),retractall(entity_descr(Entidad,_PosVieja))),
		
	%Si percibe una entidad en el piso que estaba en posesión de otra entidad
	%Se olvida de esa posesión y cualquier posición (vector) anterior de esa entidad.
	forall((member(at(Entidad,_PosNueva),Percepcion),has(_EntVieja,Entidad)),(retractall(has(_EntVieja,Entidad)),retractall(atPos(Entidad,_VectViejo)))),
	
	%Si percibe una entidad en el piso que estaba en otro lugar del piso
	%Se olvida de la ubicación anterior y de la posición (vector) anterior.
	forall((member(at(Entidad,_PosNueva),Percepcion),at(Entidad,_PosVieja)),(retractall(at(Entidad,_PosVieja)),retractall(atPos(Entidad,_VectViejo)))),
	
	%Si percibe una entidad en una posición (vector) que estaba en otra posición (vector)
	%Se olvida de la ubicación anterior y de la posición (vector) anterior.
	forall((member(atPos(Entidad,_VectNuevo),Percepcion),atPos(Entidad,_VectViejo)),(retractall(at(Entidad,_PosVieja)),retractall(atPos(Entidad,_VectViejo)))),
	
	%Si percibe que una entidad, que creia que estaba en el piso y en otra posición (vector), es poseida por otra entidad
	%Olvida que estaba en el piso y su posición (vector) anterior.
	forall((member(has(_EntNueva,Entidad),Percepcion),at(Entidad,_PosVieja),atPos(Entidad,_VectViejo)),(retractall(at(Entidad,_PosVieja)),retractall(atPos(Entidad,_VectViejo)))),
	
	%Si percibe una entidad que ya habia visto antes
	%la olvida para actualizar su posesión.
	forall((member(has(Entidad,_EntNueva),Percepcion),has(Entidad,_EntVieja)),retractall(has(Entidad,_EntVieja))),
		
	%Olvida el tiempo
	retractall(time(_)),
	
	/*
		El agente continua recordando todo lo percibido
	*/
	
	%Por cada relación percibida
	%La recuerda.
	forall(member(Relacion,Percepcion),assert(Relacion)).

	

/* 
igual al anterior pero desglozado
olvidar(node(Id,_,_)) :-
	at(_,Id),
	retractall(at(_,Id)).

olvidar(node(_,Pos,_)) :-
	atPos(_,Pos),
	retractall(atPos(_,Pos)).
*/



	
	
	
	
	
	
	