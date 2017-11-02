%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_strips, module_path_finding, extras_for_agents].

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

      ag_name(_AgName),

      %tell('log.txt'),

      update_beliefs(Percept),

      display_ag, nl,!,

      deliberate,  % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCIÓN DE UN PLAN PARA LA INTENCIÓN
		   % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCIÓN.

      planning_and_execution(Action),

      do_action(Action),

      run,!.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%   1. UPDATING BELIEFS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     FROM PERCEPTIONS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% Se encuentra definido en el módulo 'module_beliefs_update'.

% << DESARROLLADO EN ETAPA 1 >>




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     2. DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deliberate
%
% El agente analiza si continuará con su intención actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intención actual fue lograda, etc.
% En caso de no continuar con la intención corriente, establece cual
% será la nueva intención analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-

	once(high_priority(HPDesire, _Explanation)),	 % Si existe un deseo HPDesire de alta prioridad:
						         % (NO es un <<<CHOICE POINT>>>: una falla en la
						         % siguiente línea no debería buscar alternativas).

	not(intention(HPDesire)),        % y no es el caso que coincide con la intención actual,

	%write('High-priority Desire: '), %write(HPDesire), %write(', since '),
        %%writeln(Explanation), nl,

	retractall(intention(_)),     % (Estratégicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	      %	 ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                              %  para la anterior), la intención anterior se elimina y se hace assert de la nueva).

	assert(intention(HPDesire)),     % se establece HPDesire como intención actual.
	assert(plan([HPDesire])).

deliberate:-       % Si

	(   not(intention(_))
                             % actualmente no hay intención
	    %writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int)                         % la intención corriente fue lograda
            %write('Intention '), %write(Int), %writeln(' achieved.')
	;					   % o

	    plan([])                              % el plan para para la intención actual fue consumido
	    %writeln('Plan consumed.')
	;                                          % o
	    (

	        plan(Plan), Plan \= [], not(feasible(Plan))   % el plan corriente se tornó no factible, o

		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin éxito, el (sub) plan para la
	                                                      % siguiente acción de alto nivel (falla el next_primitive_action).
	    )
	    %writeln('Current plan became infeasible.'), nl
	),

	!,

	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	%write('Desires: '), %writeln(Desires),nl,
	select_intention(NewInt, _NewExpl, Desires),   % Seleccionar una intención
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	%write('New Intention: '), %write(NewInt), %write(', since '), %writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrategicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intención anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intención seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se continúa con la intención y plan corrientes


deliberate:-            % Si llega acá significa que falló el next_primitive_action al planificar la siguiente acción
	                % de alto nivel de un plan factible. Ejecuto nuevamente el delibarate.
	deliberate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  2.1. COMPUTING DESIRES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Metas / Deseos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% desire(-Desire, -Explanation)
%
% Retorna un deseo Desire (actualmente activo de acuerdo a las creencias
% del agente). Asociado al deseo se retorna una explicación,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma más básica Explanation puede ser un
% string con una descripción narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.

%_____________________________________________________________________
%
% Get potion at position
%
% Si recuerdo que una pocion dado se encuentra tirado en el piso, tener
% esa pocion es una meta.

desire(get([potion, TrName]), 'quiero apoderarme de todas las pociones!!'):-
	at([potion, TrName], _PosTr).



%_____________________________________________________________________
%
% Get treasure en tumba
%
% Si recuerdo que un tesoro dado se encuentra tirado en el piso, tener
% ese tesoro es una meta.

desire(get([gold, TrName]), 'quiero apoderarme de muchos tesoros!'):-
	has([grave,_Tumba],[gold, TrName]).



%_____________________________________________________________________
%
% Get treasure at position
%
% Si recuerdo que un tesoro dado se encuentra tirado en el piso, tener
% ese tesoro es una meta.

desire(get([gold, TrName]), 'quiero apoderarme de muchos tesoros!'):-
	at([gold, TrName], _PosTr).


%_____________________________________________________________________
%
% Rest


desire(rest, 'quiero estar descansado'):-
	property([agent, me], life, St),
	St < 100.

%_____________________________________________________________________
%
% Depositar tesoros

desire(depositarTesoro(Tesoro), 'quiero depositar todos mis tesoros!'):-
      has([agent,me],[gold,Tesoro]).


desire(ir_a_casa,'quiero defender mi casa!').

%_____________________________________________________________________
%
% Move at Random
%

desire(move_at_random, 'quiero estar siempre en movimiento!').

desire(defenderme_de([agent,UnAgente]), 'me estan atacando! debo decidir que hacer'):-

      property([agent,me],home,MiCasa),
      property([agent,UnAgente],home,CasaEnemigo), % Hay algun agente en el mapa?

      UnAgente \= me,
      MiCasa \= CasaEnemigo, % Es un enemigo?

      atPos([agent,me],MiPos),
      atPos([agent,UnAgente],SuPos),
      % Esta en rango de ataque?
      pos_in_attack_range(MiPos,SuPos).


desire(explorar, 'quiero explorar todo el mapa!'):-
      nodosNoExplorados([]).

% << TODO: DEFINIR OTROS DESEOS >>
%
% ACLARACIÓN: Pueden modificarse los deseos considerados, y la
% implementación actual de desire/2, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intención actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% Análogamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicación, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intención.
% En su forma más básica Explanation puede ser un string con una
% descripción narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.
:- dynamic high_priority/2.

/*
high_priority(rest, 'necesito descansar urgentemente D:'):-  % runs low of stamina

	property([agent, me], life, St),
	St < 30, % running low of stamina...

	once(at([inn, _HName], _Pos)). % se conoce al menos una posada

*/


high_priority(defenderme_de([agent,UnAgente]), 'hay un enemigo cerca! debo decidir que hacer'):-

      property([agent,me],home,MiCasa),
      property([agent,UnAgente],home,CasaEnemigo), % Hay algun agente en el mapa?

      UnAgente \= me,
      MiCasa \= CasaEnemigo, % Es un enemigo?

      property([agent,UnAgente],life,HPEnemigo),
      HPEnemigo > 0,

      atPos([agent,me],MiPos),
      atPos([agent,UnAgente],SuPos),

      % Esta en rango de ataque?
      writeln('Hay alun enemigo cerca?'),
      pos_in_attack_range(MiPos,SuPos),
      writeln('Hay enemigos cerca!!!!!').

high_priority(rest, 'necesito descansar'):-  % runs low of stamina

	property([agent, me], life, St),
	St < 60, % running low of stamina...

	once(at([inn, _HName], _Pos)). % se conoce al menos una posada




% << TODO: DEFINIR >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% high_priority/2, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  2.2. SELECTING INTENTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% select_intention(-Intention, -Explanation, +CurrentDesires).
%
% Selecciona uno de los deseos corrientes del agente como intención.
%
% En su forma más básica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intención al primero en este orden
% de prioridad.
%
% Tiene múltiples soluciones. Esto es adrede: si
% posteriormente a la selección de una intención (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendrá
% la siguiente intención (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.


select_intention(defenderme_de(_Agente),' quiero defenderme!',_Desires):-
      property([agent,me],home,MiCasa),
      property([agent,UnAgente],home,CasaEnemigo), % Hay algun agente en el mapa?

      UnAgente \= me,
      MiCasa \= CasaEnemigo, % Es un enemigo?

      property([agent,UnAgente],life,HPEnemigo),
      HPEnemigo > 0,

      atPos([agent,me],MiPos),
      atPos([agent,UnAgente],SuPos),
      % Esta en rango de ataque?
      pos_in_attack_range(MiPos,SuPos).

%_____________________________________________________________________
%
% Rest before commiting to any other desire
%
% Dado que el nivel de stamina es relativamente bajo, se decide ir
% descansar antes de abordar otro deseo.

select_intention(rest, 'voy a recargar antes de encarar otro deseo', Desires):-
	member(rest, Desires),
	property([agent, me], life, St),
	St < 80.



%_____________________________________________________________________
%
% Conseguir una pocion que se halla tirado en el suelo
%
% De todos los posibles objetos tirados en el suelo que el agente desea
% tener, selecciono como intención obtener aquel que se encuentra más
% cerca.


select_intention(get([potion,Pocion]), 'es el objeto más cercano de los que deseo obtener', Desires):-

	findall(ObjPos,(member(get([potion,Pocion]), Desires),at([potion,Pocion], ObjPos)),Metas),
        % Obtengo posiciones de todos los objetos meta tirados en el suelo.

	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos),
	member(get([potion,Pocion]), Desires),
        at([potion,Pocion], CloserObjPos).

%_____________________________________________________________________
%
% Conseguir un objeto mas cercano
%
% De todos los posibles objetos que el agente desea
% tener, selecciono como intención obtener aquel que se encuentra más
% cerca.

select_intention(get(Obj), 'es el objeto mas cercano (en tumba o no)', Desires):-


	findall(PosT,
                (member(get(Obj), Desires), has([grave,Tumba], Obj),
                 at([grave,Tumba],PosT))
               ,PosTumbas),

        findall(PosO,
                (member(get(Obj), Desires), at(Obj,PosO))
               ,PosObj),

        append(PosObj,PosTumbas,PosMetas),

        % Obtengo posiciones de todos las tumbas que tienen tesoros y tesoros en el piso.

	buscar_plan_desplazamiento(PosMetas, _Plan, ObjMasCercano),

	member(get(Obj), Desires),
        %y ...
        (
             % Esta en una tumba
            (has([grave,Tumba],Obj),at([grave,Tumba],ObjMasCercano) )
            ; % O
             % Esta en el piso
            at(Obj,ObjMasCercano)
        ).

%_____________________________________________________________________
%
% Depositar mis tesoros
%

select_intention(depositarTesoro(Tesoro), 'quiero depositar todos mis tesoros!', Desires):-
      has([agent,me],[gold,Tesoro]),
      member(depositarTesoro(Tesoro), Desires).


select_intention(explorar,' voy a explorar todo el mapa',Desires):-
      member(explorar,Desires).



%_____________________________________________________________________
%
% Rest
%
% Si no existen objetos que deseen obtenerse, y existe el deseo de
% descansar se decide ir a descansar.

select_intention(rest, 'no tengo otra cosa más interesante que hacer', Desires):-
	member(rest, Desires).



select_intention(ir_a_casa, 'no tengo otra que hacer, voy a defender mi casa', Desires):-
      member(ir_a_casa,Desires).

%_____________________________________________________________________
%
% Move at random

select_intention(move_at_random, 'no tengo otra cosa más interesante que hacer', Desires):-
	member(move_at_random, Desires).



% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% select_intention/3, si se lo considera apropiado.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intención Intention fue alcanzada. Esto es, si
% se verifica de acuerdo a las creencias del agente.


achieved(rest):-
	property([agent, me], life, St),
	property([agent, me], lifeTotal, MaxSt),
	AlmostMaxSt is MaxSt - 10,
	St > AlmostMaxSt.

achieved(get(Obj)):-
	has([agent, me], Obj).

achieved(goto(Pos)):-
	at([agent, me], Pos).

achieved(drop(Obj)):-
      %at([agent, me], MiPos),
      %at(Obj,MiPos).
      not(has([agent,me],Obj)).

achieved(depositarTesoro(Tesoro)):-
      %not(has([agent,me],[gold,Tesoro])),
      property([agent,me],home,MiCasa),
      at([home,MiCasa],PosCasa),
      at([agent,me],PosCasa),
      has([home,MiCasa],[gold,Tesoro]).

achieved(abrirEntidad(NombreE)):-
      at([Entidad,NombreE],Pos),
      Entidad \= agent,
      at([agent,me],Pos),
      property([agent,me]
              ,lastAction
              ,cast_spell( open([Entidad,NombreE],[potion,Pocion]) ,_TiempoAccion)),

      retract(has([agent,me],[potion,Pocion])).

achieved(huir(Agente)):-
      atPos([agent,me],MiPos),
      atPos(Agente,PosEnemigo),
      not(pos_in_attack_range(MiPos,PosEnemigo)).

achieved(atacar(Agente)):-
      property([agent,me]
              ,lastAction
              ,[attack(Agente),_TiempoAccion]).

achieved(dormir(Agente,Pocion)):-
      property(Agente,life,HPEnemigo),
      HPEnemigo is 0,
      property([agent,me]
              ,lastAction
              ,cast_spell( sleep(Agente,Pocion) ,_TiempoAccion)),
      retract(has([agent,me],Pocion)).

achieved(defenderse_de(Agente)):-
      achieved(atacar(Agente)) ;
      achieved(huir(Agente)) ;
      achieved(dormir(Agente,_Pocion)).

achieved(explorar):-
      nodosNoExplorados([]).


% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% achieved/1, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      3. PLANNING         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       & EXECUTION        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planning_and_execution(-Action)
%
% Obtiene la siguiente acción primitiva Action correspondiente al plan
% actual, removiéndola del plan. Nótese que la siguiente acción
% del plan podría ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),      % Ejecutar siguiente acción del plan.

	%write('Following plan: '), %writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	%write('Next action: '), %writeln(Action),
	assert(plan(RestOfPlan)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librería de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acción HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecución equivalga al efecto de la
% acción HLAction.
%
% Debe definirse una regla de este predicado por cada acción de alto
% nivel considerada por el agente (incluída las intenciones, que
% constituyen las acciones de más alto nivel).
%
% La planificación de HLAction puede realizarse,
% según el tipo de la acción, de las siguientes maneras:
%
%   a) simplemente especificando en forma "estática" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de búsqueda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posición determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementación es provista por la cátedra), adecuado para
%      realizar planificaciones de metas más complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opción a admite la especificación de planes recursivos, donde en
% particular, la última acción del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificación de HLAction es [Action, HLAction], donde Action es
% una acción que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acción sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acción, y
% así siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acción para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificación de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregará soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar según la acción que se esté planificando,
% si es deseable brindar soluciones alternativas.


planify(explorar,Plan):-
      nodosNoExplorados(Nodos),
      random_member(UnNodo,Nodos),
      Plan=[goto(UnNodo)].


planify(get(Obj), Plan):- % Planificación para obtener de un objeto que yace en el suelo
	at(Obj, Pos),
	Plan = [goto(Pos), pickup(Obj)].

planify(get(Obj),Plan):- % Planificación para obtener un objeto en una tumba
      has([Entidad,NombreE],Obj),
      Entidad \= agent,
      Plan=[abrirEntidad(NombreE),get(Obj)].

planify(ir_a_casa,Plan):- % Planificación para ir a la home y defender
      property([agent,me],home,MiCasa),
      at([home,MiCasa],PosCasa),
      Plan=[goto(PosCasa)].


planify(goto(PosDest), Plan):- % Planificación para desplazarse a un destino dado
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda),
	!. % Evita la búsqueda de soluciones alternativas para un plan de desplazamiento.


planify(rest, Plan):- % Planificación para desplazarse a un destino dado

	findall(Pos,at([inn, _H], Pos),Posadas),

	buscar_plan_desplazamiento(Posadas,_Plan,PosadaMasCercana),

	Plan = [goto(PosadaMasCercana), stay].


planify(stay, [noop , stay]).                     % Planificación recursiva. En este caso permite repetir indefinidamente
                                                  % una acción (noop) hasta que la intención de alto nivel corriente
                                                  % (rest) sea lograda (achieved/1). Esto se hizo así dado que resulta
                                                  % más simple que predecir de antemano cuantos turnos exactamente debe
                                                  % permanecer el agente para recargarse por completo (nótese que el agente
						  % podría incluso sufrir ataques mientras está en la posada, siendo imposible
						  % planificar de antemano cuantos turnos debe permanecer en la posada para
						  % reponerse por completo)


planify(move_at_random, Plan):- % Planificación para moverse aleatoriamente

      findall(Nodo, node(Nodo,_Vector,_Vecinos), PossibleDestPos),

      random_member(DestPos, PossibleDestPos),% Selecciona aleatoriamente una posición destino.
				              % <<<CHOICE POINT>>> (Posibilidad de backtracking)
      Plan = [goto(DestPos)].

planify(drop(Obj),Plan):-
      has([agent,me],Obj),
      Plan=[drop(Obj)].

%OJO. Tesoro es el NOMBRE del tesoro, no es una entidad
planify(depositarTesoro(Tesoro),Plan):-
      has([agent,me],[gold,Tesoro]),
      property([agent,me],home,MiCasa),
      at([home,MiCasa],PosCasa),
      %at([agent,me],PosCasa),
      Plan=[goto(PosCasa),drop([gold,Tesoro])].


planify(abrirEntidad(NombreE),Plan):- %Cuando tengo una pocion
      at([Entidad,NombreE],Pos),
      Entidad \= agent,
      has([agent,me],[potion,Pocion]),
      Plan=[goto(Pos),cast_spell(open([Entidad,NombreE],[potion,Pocion]))].

planify(abrirEntidad(NombreE),Plan):- %Cuando no tengo una pocion
      at([Entidad,NombreE],PosE),
      Entidad \= agent,
      not(has([agent,me],[potion,Pocion])),

      findall(Pos,at([potion, _Pot], Pos),Pociones),
      buscar_plan_desplazamiento(Pociones,_Plan,PocionMasCercana),

      at([potion,Pocion],PocionMasCercana),

      Plan=[get([potion,Pocion]),goto(PosE),cast_spell(open([Entidad,NombreE],[potion,Pocion]))].


calcularNodoParaHuir(MiPos,PosEnemigo,NodoAlejado):-
      PosEnemigo > MiPos, % El enemigo se encuentra en el sector de arriba a la derecha
      node(MiPos,VectorNodo,_), % Obtengo el vector de mi nodo actual
      % Obtengo todos los nodos que estan a distancia 11
      findall(Nodo,(node(Nodo,Vector,_),distance(Vector,VectorNodo,D)),NodosCercanos),
      D =< 11,
      % Ordeno los nodos cercanos de menor a mayor
      sort(NodosCercanos,[NodoAlejado|_Resto]),!.
calcularNodoParaHuir(MiPos,PosEnemigo,NodoAlejado):-
      PosEnemigo =< MiPos, % El enemigo se encuentra en el sector de abajo a la izquierda
      node(MiPos,VectorNodo,_), % Obtengo el vector de mi nodo actual
      % Obtengo todos los nodos que estan a distancia 11
      findall(Nodo,(node(Nodo,Vector,_),distance(Vector,VectorNodo,D)),NodosCercanos),
      D =< 11,
      % Ordeno los nodos cercanos de menor a mayor
      sort(NodosCercanos,[NodoAlejado|_Resto]),!.
calcularNodoParaHuir(MiPos,_PosEnemigo,NodoAlejado):- % Huir a un nodo random
      node(MiPos,VectorNodo,_), % Obtengo el vector de mi nodo actual
      % Obtengo todos los nodos que estan a distancia 11
      findall(Nodo,(node(Nodo,Vector,_),distance(Vector,VectorNodo,D)),NodosCercanos),
      D =< 11,
      random_member(NodoAlejado,NodosCercanos).



planify(huir(Agente),Plan):-
      %write('Planificando atacar '),writeln(Agente),
      %at(Agente,PosEnemigo),
      %at([agent,me],MiPos),
      Agente \= [agent,me],
      property([agent,me],home,MiCasa),
      at(MiCasa,Nodo),
      %calcularNodoParaHuir(MiPos,PosEnemigo,NodoAlejado),
      writeln('Voy a huir hacia mi casa'),
      Plan=[goto(Nodo)].

planify(atacar(Agente),Plan):-
      %write('Planificando atacar '),writeln(Agente),
      atPos(Agente,PosEnemigo),
      atPos([agent,me],MiPos),
      Agente \= [agent,me],
      %writeln('Esta en rango de ataque?'),
      pos_in_attack_range(MiPos,PosEnemigo),
      %writeln('No esta en rango de ataque'),
      Plan=[attack(Agente)].

planify(dormir(Agente,Pocion),Plan):-
      %write('Planificando dormir a '),write(Agente),write(' con '),writeln(Pocion),
      atPos(Agente,PosEnemigo),
      atPos([agent,me],MiPos),
      Agente \= [agent,me],
      %writeln('Esta en rango de ataque?'),
      pos_in_attack_range(MiPos,PosEnemigo),
      %writeln('No esta en rango de ataque'),
      Plan=[cast_spell(sleep(Agente,Pocion))].




planify(defenderme_de(Agente),Plan):-

      property([agent,me],life,MiHP),
      property(Agente,life,HPEnemigo),
      % Obtuve la vida de cada agente

      property([agent,me],skill,MiXP),
      property(Agente,skill,XPEnemigo),
      % Obtuve la experiencia de cada agente

      findall([potion,Pocion],has([agent,me],[potion,Pocion]),MisPociones),
      findall([potion,Pocion],has(Agente,[potion,Pocion]),PocionesEnemigo),
      % Obtuve las pociones de cada agente

      % Decido el plan en base a estos datos
      decidir_AtacarHuir(MiHP,MiXP,MisPociones,Agente,HPEnemigo,XPEnemigo,PocionesEnemigo,Plan).



decidir_AtacarHuir(MiHP,_MiXP,[],Agente,HPEnemigo,XPEnemigo,[],Plan):-
      % Peor caso: el enemigo tiene mucha mas vida que yo
      HPNueva is MiHP - 100 - XPEnemigo,
      HPNueva < HPEnemigo,
      writeln('Decido atacar o huir: Ninguno tiene pociones, yo tengo muy poca vida'),
      Plan=[huir(Agente)],!.

decidir_AtacarHuir(_MiHP,MiXP,[],Agente,_HPEnemigo,XPEnemigo,[],Plan):-
      % Caso en que ninguno tiene pociones: calculo posible ataque
      FuerzaA is MiXP + 50,
      ResistA is XPEnemigo + 50,
      % Si el ataque es malo huyo
      FuerzaA < ResistA,
      writeln('Decido atacar o huir: Ninguno tiene pociones, calculo un ataque y es malo'),
      Plan=[huir(Agente)],!.

decidir_AtacarHuir(_MiHP,MiXP,[],Agente,_HPEnemigo,XPEnemigo,[],Plan):-
      % Caso en que ninguno tiene pociones: calculo posible ataque
      FuerzaA is MiXP + 50,
      ResistA is XPEnemigo + 50,
      % Si el ataque es malo huyo
      FuerzaA >= ResistA,
      writeln('Decido atacar o huir: Ninguno tiene pociones, calculo un ataque y es bueno'),
      Plan=[atacar(Agente)],!.

decidir_AtacarHuir(_MiHP,_MiXP,[Pocion|_RestoP],Agente,_HPEnemigo,_XPEnemigo,_PocionesEnemigo,Plan):-
      % Caso en el que yo tengo una pocion
      Plan=[dormir(Agente,Pocion)],
      writeln('Decido atacar o huir: yo tengo al menos una pocion, lo duermo')
      ,!.

decidir_AtacarHuir(_MiHP,_MiXP,[],Agente,_HPEnemigo,_XPEnemigo,[_Pocion|_RestoPocion],Plan):-
      % Caso en que ninguno tiene pociones: calculo posible ataque
      Plan=[huir(Agente)],
      writeln('Decido atacar o huir: yo no tengo pociones pero el enemigo si, huyo').

decidir_AtacarHuir(_MiHP,_MiXP,_MisPociones,Agente,_HPEnemigo,_XPEnemigo,_PocionesEnemigo,[huir(Agente)]).


nodosNoExplorados(ListaNodos):-
      findall(IDNodo,(node(_ID,_Vector,Ady),member([IDNodo,_Costo],Ady)),NodosAdy),
      findall(Nodo,(member(Nodo,NodosAdy),not(node(Nodo,_V,_A))),NodosNoExplorados),
      sort(NodosNoExplorados,ListaNodos).

% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% planify/2, si se lo considera apropiado.

%AGREGADO
escribirCreencias(Archivo):-
	tell(Archivo),
	write("--------------Comienzo--------------"),nl,
	forall(at(Ent,Nod), (write(at(Ent,Nod)),nl)),nl,
	forall(atPos(Ent,Vec), (write(atPos(Ent,Vec)),nl)),nl,
	forall(has(Ent1,Ent2), (write(has(Ent1,Ent2)),nl)),nl,
	forall(entity_descr(Entidad,Desc), (write(entity_descr(Entidad,Desc)),nl)),nl,
	forall(node(Id,Pos,Ady), (write(node(Id,Pos,Ady)),nl)),nl,
	write("-----------------Fin----------------"),nl.

escribirNodos(Archivo):-
	tell(Archivo),
	write("--------------Comienzo--------------"),nl,
	forall(node(Id,_Pos,_Ady), (write(Id),nl)),nl,
	write("-----------------Fin----------------"),nl.

cantNodos(N):-
	findall(Id,node(Id,_Pos,_Ady),Lista),
	longitud(Lista,N).


longitud([],0).
longitud([_|Xs],N):- longitud(Xs,K), N is K+1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acción primitiva del plan de alto
% nivel, además del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallará mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versión refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% [A_2, ..., A_n].
%
% Observación: A modo de mantener registro de la descomposición de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el propósito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando ésta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificación de cuándo Action se considera lograda.

	achieved(Action), % Action fue lograda.
	!,
	%write('Action '), %write(Action), %write(' achieved.'),nl,
	remove_executed_ancestors(RestOfPlan, CleanRestOfPlan),
	next_primitive_action(CleanRestOfPlan, NextAction, RemainingPlan).


% CB:

next_primitive_action([Action | RemainingPlan], Action, CleanRemainingPlan):-
	primitive(Action),
	remove_executed_ancestors(RemainingPlan, CleanRemainingPlan),
	!.

% CR:

next_primitive_action([HLAction | RestOfPlan], Action, RemainingPlan):-


        if_fails_do(

	clause(planify(HLAction, _SubPlan), _), % Planificación definida para HLAction.

		    throw_exception((
			  write(HLAction),
			  write(' is undefined. Declare it as primitive or planify it.')
			 ))
		   ),
        !,

	(

	     planify(HLAction, SubPlan)	 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	     ;

	     (write('Planning for '), write(HLAction), write(' failed.'), nl, fail)
	                                 % Si definitivamente no encuentra plan para la intención seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intención mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acción de alto nivel coincide con la última del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%%write(HLAction), %write(' -- expanded into -> '), %write(SubPlan),nl,
	%writeln('          -- expanded into -> '), nl,
	%write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observación: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acción de
% RestOfPlan.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove_executed_ancestors(+Plan, -CleanPlan)
%
%

remove_executed_ancestors([[_]| Rest], Clean):-
	!,
	remove_executed_ancestors(Rest, Clean).

remove_executed_ancestors(Clean, Clean).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% primitive(ActionName).
%
% Especifica las acciones primitivas del agente, es decir, aquellas que
% no pueden descomponerse en acciones más básicas.

primitive(move(_)).
primitive(pickup(_)).
primitive(drop(_)).
primitive(attack(_)).
primitive(cast_spell(_)).
primitive(noop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% feasible(+Plan).
%
% Determina si el plan jerárquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con éxito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra más
        % en la posición que recordaba), entonces project/3 fallará, reflejando que el plan no es factible.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = miguelito,
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
                    AgClassName = pepito,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).













































































