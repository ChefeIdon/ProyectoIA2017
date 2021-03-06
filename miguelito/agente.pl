%% Agente Miguelito
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      %writeln(Percept),

      update_beliefs(Percept),

      display_ag, nl,

      decide_action(Action),

      do_action(Action),

      write('Action: '), writeln(Action), nl, nl,

      run.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Es verdadero si Destino se unifica con un nodo donde se encuentra un
% tesoro
esMetaAlcanzable(Destino):-
      at([gold,_NombreTesoro],Destino).


% Decide una accion de un plan si el Destino del plan (Meta) es
% alcanzable
decide_action(Action):-
      plan([Action|RestoPlan],Destino),
      write('DA| Estoy ejecutando un plan para llegar a '),write(Destino),nl,
      write('DA| La meta es alcanzable?... '),

      esMetaAlcanzable(Destino),
      retractall(plan(_,_)),

      write('Si!, entonces la accion que voy a tomar es:'), write(Action),nl,
      write('DA| El resto del plan: '),write(RestoPlan),nl,
      assert(plan(RestoPlan,Destino)).


decide_action(Action):-
	at([agent, me], MyNode),
	at([gold, GName], MyNode),
	write('DA| Encontré un tesoro: '), write(GName), write('!'),
        write(' voy a intentar tomarlo...'),nl,
        Action = pickup([gold, GName]).

decide_action(Action):-
	atPos([agent, me], MyPos),
	atPos([agent, Target], TPos),
	Target \= me,
	property([agent, Target], life, TLife),
	TLife > 0,
	pos_in_attack_range(MyPos, TPos),
        write('DA| Hay un enemigo cerca! voy a intentar atacarlo...'),nl,
	Action = attack([agent, Target]).

% Llama a buscar_plan_desplazamiento con una lista de todos los nodos
% que conoce el agente donde hay un tesoro
decide_action(Action):-
      write('DA| Voy a buscar los tesoros que conozco'),nl,
      findall(Node,at([gold,_NombreOro],Node),NodosMeta),
      buscar_plan_desplazamiento(NodosMeta,[Action|RestoPlan],Destino),
      write('DA| Encontre un plan para llegar a un tesoro en el nodo: '),write(Destino),nl,
      write('DA| Mi siguiente accion es: '),write(Action),nl,
      assert(plan(RestoPlan,Destino)).


decide_action(Action):-
      at([agent, me], MyNode),
      findall(Node, ady(MyNode, Node), PossibleDestNodes),
      random_member(DestNode, PossibleDestNodes), % Selecciona aleatoriamente una posición destino.
      write('DA| Me voy a mover al azar al nodo: '),write(DestNode),nl,
      Action = move(DestNode).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic ag_name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:-
           %set_prolog_stack(global, limit(512000000)),
           AgName = miguelito,
           agent_init(AgName),
           assert(ag_name(AgName)),
		   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.


start_ag_instance(InstanceID):-
                    %set_prolog_stack(global, limit(512000000)),
                    AgClassName = miguelitos,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).











