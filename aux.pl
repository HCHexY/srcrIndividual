/** Predicados de gestão da instãncia**/
/** Carregar Grafo **/
loadGraph(grafo(Nodes,Arestas)):-
    findall(G, grafo(G,_,_), Bag),
    length(Bag, Int),
    NGid is Int+1,
    assert(grafo(NGid,Nodes,Arestas)),
    assert(nodes(Nodes)),
    sort(Arestas,Noreps),
    maplist(assert,Noreps).
loadGraph(Gid):-
    grafo(Gid,Nodes,Arestas),
    assert(nodes(Nodes)),
    sort(Arestas,Noreps),
    maplist(assert,Noreps).
/** Descarregar Grafo **/
unloadGraph(Gid):-
    grafo(Gid,Nodes,Arestas),
    retract(nodes(Nodes)),
    maplist(retract,Arestas).
/**Predicado de negação**/
not(P) :- P, !, fail.
not(P).
/**Predicado de sempre verdadeiro**/
true(X).
/** 
 * Predicado para extrair os primeiros I elementos da min-heap
 * **/
truncateHeap(_,0,[]).
truncateHeap(Hn,I,[Key|L]):-
    get_from_heap(Hn, _, Key, Heap),
    Int is I-1,
    truncateHeap(Heap,Int,L).
%-----------------------------------------

/**
 * Perdicados de teste das paragens
 * **/
/** Não pertence ás operadoras Banidas **/
avoidOperadoras(X):-
    paragem(X,_,_,_,_,_,Op,_,_,_,_),
    opList(L),
    not(member(Op,L)).
/** Pertence ás operadoras permitidas **/
selectOperadoras(X):-
    paragem(X,_,_,_,_,_,Op,_,_,_,_),
    opList(L),
    member(Op,L).
/** Tem publicidade **/
hasPublicidade(X):- paragem(X,_,_,_,_,"Yes",_,_,_,_,_).
/** Tem Abrigo **/
isAbrigado(X):-paragem(X,_,_,_,"Sem Abrigo",_,_,_,_,_,_),!,fail.
isAbrigado(X).
/** Passa por todos os checkPoints **/
checksPoints(Sol):-
    cp(L),
    remove_elements(L,Sol,Ret).
checkPointsAux(_,[]).
checkPointsAux([H|T],L):-
    (member(H,L),delete(L,H,L2),!,checkPointsAux(T,L2);
    !,checkPointsAux(T,L)).
%---------------------------------------
remove_elements(L, [H|T], R) :-
    delete(L, H, R1),
    remove_elements(R1, T, R).
remove_elements([], _, []).
%------------------------------------------
cost([X],5).
cost([H|T],C):-
    cost([H],Ch),
    cost(T,Cn),
    C is Ch + Cn.
min( X, Y, X) :-
    X=< Y,!.
min( X, Y, Y).
biggest(Big):-
    nodes(L),
    length(L,Int),
    Big is Int*Int*Int.
euler(N,M,T):-
    paragem(N,X1,Y1,_,_,_,_,_,_,_,_),
    paragem(M,X2,Y2,_,_,_,_,_,_,_,_),
    Xd is X1-X2,Yd is Y1-Y2,
    T is sqrt((Xd*Xd) +(Yd*Yd)).
aresta(N,M,T):-
    aresta(N,M),
    h(N,M,T).

%checkPoints([1,2,3]).
%checksPoints([12345]).
%solve_bft_chk(110,277,[257,276,200],S), write(S), checksPoints(S).
