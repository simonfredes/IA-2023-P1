:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.
%	
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan).
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%
	
buscarEstrella(Frontera, Metas, Camino, Costo, Destino):-
	buscar(Frontera, [], Metas, Destino),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo),
	!.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), 	% genera los vecinos del nodo - TO-DO
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(Vecinos, Nodo, Metas, FronteraSinNodo, NuevosVisitados, NuevaFrontera, NuevosVisitadosAux),  % agrega vecinos a la frontera - TO-DO
	quick_sort(NuevaFrontera, Metas, NuevaFronteraAux),
	buscar(NuevaFronteraAux, NuevosVisitadosAux, Metas, MM). % continua la busqueda con la nueva frontera


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% generarVecinos(+[IdNodo, CostoNodoActual], -Vecinos) 
%
% Genera una lista con todos los nodos adyacentes a un nodo y su nuevo costo.
% Recibe un par [IdNodo, CostoPadre] y genera una lista de pares [IdNodoVecino, Costo] por cada nodo adyacente al nodo con id IdNodo. 
% IdNodo es el id de un nodo adyacente y Costo es el costo del nodo adyacente sumado al costo del nodo padre (CostoNodoActual).

generarVecinos(Nodo, Vecinos) :-
    Nodo = [Id, C],
    node(Id, _, _, _, Conexiones),
    generarVecinosAux(Conexiones, C, Vecinos).

generarVecinosAux([], _, []).  								% Caso base: no hay vecinos, la lista resultante es vacía.
generarVecinosAux([[IdNodo, Costo] | RestoConexiones], C, [NodoVecino | RestoVecinos]) :- %Si la lista no es vacia:
    NuevoCosto is Costo + C,  % Suma el costo original con C.
    NodoVecino = [IdNodo, NuevoCosto],
    generarVecinosAux(RestoConexiones, C, RestoVecinos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar(+Vecinos, +NodoPadre, +Metas, +Frontera, +Visitados, -NuevaFrontera, -NuevosVisitados) 
%
% 

agregar([], _, _, Frontera, Visitados, NuevaFrontera, VisitadosRetorno):-
	NuevaFrontera = Frontera,
	VisitadosRetorno = Visitados.
agregar([NodoVecino|Vecinos], NodoPadre, Metas, Frontera, Visitados, FronteraRetorno, VisitadosRetorno):-
	agregarAux(NodoVecino, NodoPadre, Metas, Frontera, Visitados, NuevaFrontera, NuevosVisitados),
	agregar(Vecinos, NodoPadre, Metas, NuevaFrontera, NuevosVisitados, FronteraRetorno, VisitadosRetorno).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregarAux(+NodoVecino, +NodoPadre, +Metas, +Frontera, +Visitados, -NuevaFrontera, -NuevosVisitados) 
%
% Aplica el algoritmo A* a un nodo y devuelve la nueva Frontera y la nueva lista de nodos visitados 
% según cual sea el caso en que el predicado tenga exito.


% el nodo vecino ya está en la frontera pero su costo es menor al que tiene asociado en la frontera. 
agregarAux(NodoVecino, NodoPadre, _, Frontera, Visitados, NuevaFrontera, NuevosVisitados):-
	NodoVecino = [IdNodoVecino, Costo],
	member([IdNodoVecino, CostoEnFrontera], Frontera),
    Costo < CostoEnFrontera,
	!,
    NodoPadre = [IdNodoPadre, _], 
	delete([IdNodoVecino, CostoEnFrontera], Frontera, FronteraConElementoEliminado),
	append([[IdNodoVecino, Costo]], FronteraConElementoEliminado, NuevaFrontera),
	retractall(padre(IdNodoVecino, _)),
	assert(padre(IdNodoVecino, IdNodoPadre)),
	NuevosVisitados = Visitados.

% el nodo vecino está en la lista de visitados pero su costo es menor al que tiene asociado en la lista de visitados
agregarAux(NodoVecino, NodoPadre, _, Frontera, Visitados, NuevaFrontera, VisitadosAux):-
   NodoVecino = [IdNodoVecino, Costo],
   member([IdNodoVecino, CostoEnVisitados], Visitados),
   Costo < CostoEnVisitados,
   !,
   NodoPadre = [IdNodoPadre, _],  
   delete([IdNodoVecino, CostoEnVisitados], Visitados, VisitadosAux),
   append([[IdNodoVecino, Costo]], Frontera, NuevaFrontera),
   retractall(padre(IdNodoVecino, _)),
   assert(padre(IdNodoVecino, IdNodoPadre)).

% el nodo vecino ya está en la frontera pero el costo que tiene asociado es mayor o igual al costo que tiene asociado en la frontera
agregarAux(NodoVecino, _, _, Frontera, Visitados, NuevaFrontera, NuevosVisitados):-
   NodoVecino = [IdNodoVecino, Costo],
   member([IdNodoVecino, CostoEnFrontera], Frontera),
   Costo >= CostoEnFrontera,
   !,
   NuevaFrontera = Frontera,
   NuevosVisitados = Visitados.

% el nodo vecino está en la lista de visitados y el costo que tiene asociado es mayor o igual al que tiene asociado en la lista de visitados
agregarAux(NodoVecino, _, _, Frontera, Visitados, NuevaFrontera, VisitadosAux):-
   NodoVecino = [IdNodoVecino, Costo],
   member([IdNodoVecino, CostoEnVisitados], Visitados),
   Costo >= CostoEnVisitados,
   !, 
   NuevaFrontera = Frontera,
   VisitadosAux = Visitados.

% el nodo vecino no está ni en la lista de visitados ni en la frontera
agregarAux(NodoVecino, NodoPadre, _, Frontera, Visitados, NuevaFrontera, NuevosVisitados):-  
	NodoVecino = [IdNodoVecino, Costo],
	not(member([IdNodoVecino, _], Frontera)), 
	not(member([IdNodoVecino, _], Visitados)),
	!,
	append([[IdNodoVecino, Costo]], Frontera, NuevaFrontera),
	NodoPadre = [IdNodoPadre, _],
	retractall(padre(IdNodoVecino, _)),
	assert(padre(IdNodoVecino, IdNodoPadre)),
	NuevosVisitados = Visitados.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%

agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%

costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%

calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularF(+Nodo, +Metas, -F).
%
% calcula el valor minimo para F segun la meta mas cercana a un nodo.
%
%

calcularF(Nodo, Metas, F):-
	Nodo = [IdNodo, Costo],
    findall(CostoH, (member(Meta, Metas), calcularH(IdNodo, Meta, CostoH)), TodasLasH),
    obtenerMin(TodasLasH, H),
 	F is H + Costo.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerMin(+Lista, -Min).
%
% Recupera el menor elemento de una lista
%
%

obtenerMin([X], X).
obtenerMin([X, Y|Xs], Salida):-
	X < Y, 
	obtenerMin([X|Xs], Salida),
    !.
obtenerMin([X, Y|Xs], Salida):-
	X >= Y, 
	obtenerMin([Y|Xs], Salida).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% quick_sort(+Lista, +Metas, -ListaOrdenada).
%
% Predicado de ordenamiento.
% Recibe una lista a ordenar y la lista con todas las metas y devuelve la lista ordenada.
%
%

quick_sort([], _, []).
quick_sort([H|T], Metas, Sorted):-
	pivoting(H,T, Metas, L1,L2),
	quick_sort(L1, Metas, Sorted1),
	quick_sort(L2, Metas, Sorted2),
	append(Sorted1,[H|Sorted2], Sorted).
   

pivoting(_,[], _, [],[]).
pivoting(H,[X|T], Metas, [X|L], G):-
	calcularF(H, Metas, Fh),
	calcularF(X, Metas, Fx),
	Fx =< Fh,
	!,
	pivoting(H,T, Metas, L, G).
pivoting(H,[X|T], Metas, L, [X|G]):-
	calcularF(H, Metas, Fh),
	calcularF(X, Metas, Fx),
	Fx > Fh,
	pivoting(H,T, Metas, L,G).
