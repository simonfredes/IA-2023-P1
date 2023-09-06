:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.

%TO-DO 
% actualizar at: Si veo un nodo que antes tenia un premio, y ahora no, quitarlo de mi base de creencias.
%			 at: Ignorar los relojes y las vidas, ya que el tiempo hace que no se repitan pero son el mismo 		
% si me llega un nodo, entonces, 
%	si at(idNodo,_,_) tiene un premio entonces
%		Si mi Perc tiene un at con el mismo id de nodo
%			no hago nada
%		Si no, lo elimino de mi base de creencias
update_beliefs(Perc):-
	retractall(time(_)),
	member(time(X), Perc), assert(time(X)),
	retractall(direction(_)),
	member(direction(NewDir), Perc), 
	asserta(direction(NewDir)),

	retractall(at(_,agente,_)),
	agregar_nodes_a_lista(Perc,ListaNodes),
	agregar_at_a_lista(Perc, ListaAts),
	updateDeleted(ListaNodes,ListaAts),
	updateListaNodes(ListaNodes),
	updateListaAts(ListaAts).
	
agregar_nodes_a_lista(Perc, ListaNodes) :-
    findall(node(Id, PosX, PosY, Costo, Conexiones), member(node(Id, PosX, PosY, Costo, Conexiones), Perc), ListaNodes).

agregar_at_a_lista(Perc, ListaAts) :-
    findall(at(Obj, ObjID, ObjPos), member(at(Obj, ObjID, ObjPos), Perc), ListaAts).

updateListaAts([]).
updateListaAts([AT|Rest]):-
	update(AT),
	updateListaAts(Rest).

updateDeleted([],_).
updateDeleted(_,[]).
updateDeleted([AT|Rest],ListaAts):-
	updateDeletedNodes(AT,ListaAts),
	updateDeleted(Rest, ListaAts).


updateListaNodes([]).
updateListaNodes([AT|Rest]):-
	updateNodes(AT),
	updateListaNodes(Rest).

update(AT):- %agrego y actualizo el reloj y las vidas (el tiempo)
    AT = at(A,B,C),
    at(_,_,C),
	retractall(at(_,_,C)),
	asserta(at(A,B,C)),
	!.

update(AT):-
    AT = at(A,B,C),
    not(at(A,B,C)),
	assert(at(A,B,C)),!.


	% si me llega un nodo percibido, entonces, 				node(Id, PosX, PosY, Costo, Conexiones),	at(IdNodo, TipoEntidad, IdEntidad),
	%	si existe en mi base de creencias at(idNodo,_,_)-- (tiene un premio)
	%		Si mi Perc NO tiene un at con el mismo id de nodo
	%			Lo elimino de mis creencias
updateDeletedNodes(Node,AtList):-
	Node = node(Id,_,_,_,_),
	at(Id,_,_),
	\+member(at(Id,_,_), AtList),
	retractall(at(Id,_,_)).

updateDeletedNodes(Node,AtList):-
	Node = node(Id,_,_,_,_),
	at(Id,_,_),
	member(at(Id,_,_), AtList).

updateDeletedNodes(Node,AtList):-
	Node = node(Id,_,_,_,_),
	\+at(Id,_,_).

updateNodes(Node):-
	Node = node(Id,PosX,PosY,Costo,Conexiones),
	node(Id,PosX,PosY,Costo,Conexiones),!.

updateNodes(Node):-
	Node = node(Id,PosX,PosY,Costo,Conexiones),
	not(node(Id,PosX,PosY,Costo,Conexiones)),
	assert(node(Id,PosX,PosY,Costo,Conexiones)),!.


