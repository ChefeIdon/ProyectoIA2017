%% Agente Miguelito
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


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
decide_action(Action):-
      plan([Action|RestoPlan]),
      retractall(plan(_)),
      write('DA| Estoy ejecutando un plan, la accion que voy a tomar es:'), write(Action),nl,
      write('DA| El resto del plan: '),write(RestoPlan),nl,
      assert(plan(RestoPlan)).

decide_action(Action):-
	at([agent, me], MyNode),
	at([gold, GName], MyNode),
	write('DA| Encontr� un tesoro: '), write(GName), write('!'),
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

decide_action(Action):-
      write('DA| Voy a buscar los tesoros que conozco'),nl,
      findall(Node,at([gold,_NombreOro],Node),NodosMeta),
      buscar_plan_desplazamiento(NodosMeta,[Action|RestoPlan],Destino),
      write('DA| Encontre un plan para llegar al nodo: '),write(Destino),nl,
      write('DA| Mi siguiente accion es: '),write(Action),nl,
      assert(plan(RestoPlan)).




decide_action(Action):-
      at([agent, me], MyNode),
      findall(Node, ady(MyNode, Node), PossibleDestNodes),
      random_member(DestNode, PossibleDestNodes), % Selecciona aleatoriamente una posici�n destino.
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
% Solicita la registraci�n al juego, y recuerda su nombre.


start_ag:-
      set_prolog_stack(global, limit(512000000)),
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
% Solicita la registraci�n al juego de una instancia, y recuerda su
% nombre, que ser� el de la versi�n original seguido del InstanceID
% entre par�ntesis.


start_ag_instance(InstanceID):-
      set_prolog_stack(global, limit(512000000)),
                    AgClassName = template,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).










