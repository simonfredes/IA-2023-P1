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
	write('cantidad de metas son:'), write(CantMetas),nl,
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
	write('Es META'),nl,
	!.

buscar(Frontera, Visitados, Metas, MM):-
	write('Voy a entrar a buscar1 con: NuevaFrontera='), write(Frontera), nl,
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo -
	write('Genere los vecinos del nodo en buscar, y son: '),write(Vecinos),nl,
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera - TO-DO
	write('Voy a entrar a buscar con: NuevaFrontera='), write(NuevaFrontera), nl,
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%Nodo es una lista con identificador y costo, y los vecinos tienen estructura de node
% Genera los vecinso de un Nodo, teniendo en cuenta el costo de pasar por el nuevo nodo.
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

% Predicado para recuperar el costo de un nodo con un ID dado en una lista.
retrieveNodoFromList(Id, [[Id, CostNodo] | _], [Id,CostNodo]). % Si encontramos el nodo, retornamos su costo.

retrieveNodoFromList(Id, [_ | Resto], CostNodo) :- % Si no encontramos el nodo, seguimos buscando en el resto de la lista.
    retrieveCostFromList(Id, Resto, CostNodo).

retrieveNodoFromList(_, [], _). % Caso base: Si la lista está vacía, no se encuentra el nodo.

calcularFenFrontera([],Metas,[]).
calcularFenFrontera([ [IDNodo,CostoAcumulado] | RestoLista] , Metas, FronteraConFCalculado):-
	obtenerMejorH([IDNodo,CostoAcumulado],Metas,Resultado),
	NuevoCosto is CostoAcumulado+Resultado,
    calcularFenFrontera(RestoLista, Meta, RestoFrontera),
    FronteraConFCalculado = [[IDNodo, NuevoCosto] | RestoFrontera].

obtenerMejorH(_,[],999999).

obtenerMejorH([IDNodo,CostoAcumulado], [Meta|Metas], Resultado):-
	calcularH(IDNodo, Meta, Resultado1),
	obtenerMejorH([IDNodo,CostoAcumulado], Metas, Resultado2),
	Resultado1 >= Resultado2,
	Resultado is Resultado2.

obtenerMejorH([IDNodo,CostoAcumulado], [Meta|Metas], Resultado):-
	calcularH(IDNodo, Meta, Resultado1),
	obtenerMejorH([IDNodo,CostoAcumulado], Metas, Resultado2),
	Resultado is Resultado1.

cheaper(>,[_,C1],[_,C2]) :-
        C1>C2.

cheaper(<, [_,C1],[_,C2]) :-
        C1=<C2.

agregar(FronteraSinNodo,[], NuevaFrontera, _, NodoPadre, _):- 
	%TO-DO padre
	NuevaFrontera= FronteraSinNodo.
	%NuevosVisitados= Visitados.

agregar(FronteraSinNodo, Vecinos, NuevaFrontera, Visitados, NodoPadre, Metas):-
	Vecinos= [Cabeza | Cola],
	agregarAux(FronteraSinNodo,Cabeza, NuevaFronteraAux, Visitados, NodoPadre, Metas),
	agregar(NuevaFronteraAux, Cola, NuevaFrontera,Visitados,NodoPadre,Metas),
	%calcularFenFrontera(NuevaFrontera,Metas,FronteraConFCalculado), %Para cada nodo de la frontera, su costo será la funcion F(N) (i.e costo acumulado + euristica)
	%predsort(cheaper, FronteraConFCalculado, FronteraOrdenada), %ordeno la frontera de menor a mayor por costo,
	write('SALI DEL AGREGAR'),nl.

							%%%%%%%%%%%%%%%%%%%%%% AGREGAR AUX %%%%%%%%%%%%%%%%%%%%%%%%%

agregarAux(FronteraSinNodo,NodoVecinosActual, NuevaFrontera, _, NodoPadre, Metas):- %si vecino pertenece a frontera actual y CostoViejo MayorIgual CostoNuevo
	NodoVecinosActual= [Id, Costo],
	member([Id,CostoNodoViejo] , FronteraSinNodo),
	CostoNodoViejo>=Costo, 
	delete(FronteraSinNodo,[Id,CostoNodoViejo], FronteraNuevaAux), 
	retractall(padre(NodoVecinosActual,_)),
	assert(padre(NodoVecinosActual, NodoPadre)),
	%write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 1%%%%%%%%%%%%%%%%'),nl,
   	append(FronteraNuevaAux, [NodoVecinosActual], NuevaFrontera).	

agregarAux(FronteraSinNodo,NodoVecinosActual, NuevaFrontera, _, NodoPadre, Metas):- %si vecino pertenece a frontera actual y CostoViejo menor CostoNuevo
	%write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 2%%%%%%%%%%%%%%%%'),nl,
	NodoVecinosActual= [Id, Costo],
	%write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 2 UNO %%%%%%%%%%%%%%%%'),nl,
	member([Id,CostoNodoViejo], FronteraSinNodo),
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 2 DOS %%%%%%%%%%%%%%%%'),nl,
	CostoNodoViejo<Costo, 
	NuevaFrontera= FronteraSinNodo.

agregarAux(FronteraSinNodo,NodoVecinosActual, NuevaFrontera, Visitados, NodoPadre, Metas):- %si vecino pertenece a visitados y CostoViejo mayor CostoActual
	%write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 %%%%%%%%%%%%%%%%'),nl,
	NodoVecinosActual= [Id,Costo],
	%write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 UNO %%%%%%%%%%%%%%%%'),nl,
	member([Id,CostoViejo],Visitados),
	write('la Lsita Visitados es'), write(Visitados), nl,
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 DOS %%%%%%%%%%%%%%%%'),nl,
	write('CostoViejo es'), write(CostoViejo), nl,
	write('CostoActual es'), write(Costo),nl,
	CostoViejo>=Costo,
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 CINCO %%%%%%%%%%%%%%%%'),nl,
	delete(Visitados, NodoCostoViejo, NuevosVisitadosAux),
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 SEIS %%%%%%%%%%%%%%%%'),nl,
	append(NuevosVisitadosAux,[NodoVecinosActual], Visitados),
	retractall(padre(NodoVecinosActual,_)),
	assert(padre(NodoVecinosActual, NodoPadre)),
	write('%%%%%%%%%%%%%%%%%%%%%%%%%%%entre al agregarAUX CASO 3 SIETE %%%%%%%%%%%%%%%%'),nl.

agregarAux(FronteraSinNodo,NodoVecinosActual, NuevaFrontera, Visitados, NodoPadre, Metas):- %si vecino pertenece a visitados y CostoViejo menor CostoActual
NodoVecinosActual= [Id,Costo],
	member([Id,CostoViejo],Visitados),
	write('Entre Caso 4'),nl,
	CostoViejo<Costo,
	NuevaFrontera= FronteraSinNodo.


%TODO- cada vez q agrego a frontera, calcularF para el nodo agregando
% Y guardarlo en frontera con el costo actualizado
agregarAux(FronteraSinNodo,NodoVecinosActual, NuevaFrontera, _, NodoPadre, Metas):- %si el vecino es nuevo lo agrego a la frontera
	write('ENTRE AL CASO EN EL QUE EL VECINO ES NUEVO Y LO AGREGO A LA FRONTERA el nodo:  '),
	write(NodoVecinosActual),nl,
	write('la nueva frontera es: '),
	retractall(padre(NodoVecinosActual,_)),
	assert(padre(NodoVecinosActual, NodoPadre)),
	append([NodoVecinosActual],FronteraSinNodo, NuevaFrontera),write(NuevaFrontera),nl.

 