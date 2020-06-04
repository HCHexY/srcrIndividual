%-----------------------------------------------------------------
%   SRCR - Trabalho individual
%---------------------------------------------------------------------

:- set_prolog_flag(discontiguous_warnings,off).
:- set_prolog_flag(single_var_warnings,off).
:-dynamic paragem/12.
:-dynamic aresta/2.
:-dynamic nodes/1.
:-include('paragens.pl').
:-include('grafo.pl').
:-include('aux.pl').
:-include('scripts.pl').
:-use_module(library(lists)).

:-use_module(library(heaps)).



:- op(900,xfy,'::').

% Predicado paragem
%paragem(Gid,Lat,Long,Conservacao,Tipodeabrigo,Publicidade,Operadora,Carreiras,Codrua,Nomerua,Freguesia).
% Predicado carreira
%carreira([paragem(X)]).


%-----------------------------------------------------------------
% Simple Depth-First (1)
%-----------------------------------------------------------------
depthf(P,Goal,Goal,[Goal|P]).
depthf(Path,Nodo,Goal,Sol) :-
    aresta(Nodo, ProxNodo),
    not(member(ProxNodo, Path)),
	depthf([Nodo|Path],ProxNodo,Goal,Sol).	

%solve_df(11, 23, L).
solve_df(Nodo,Goal,L) :-
    paragem(Nodo,_,_,_,_,_,_,_,_,_,_),
	depthf([],Nodo,Goal, L).

%-----------------------------------------------------------------
% Limitado a Operadoras Depth-First (2)
%-----------------------------------------------------------------
%solve_df(11, 23, L).

depthf2(P,Goal,Goal,[Goal|P]).
depthf2(Path,Nodo,Goal,Sol) :-
    aresta(Nodo, ProxNodo),
    selectOperadoras(ProxNodo),
    not(member(ProxNodo, Path)),
	depthf2([Nodo|Path],ProxNodo,Goal,Sol).




solve_df_select(Nodo,Goal,List,L) :-
    paragem(Nodo,_,_,_,_,_,_,_,_,_,_),
    
    assert(opList(List)),
    selectOperadoras(Nodo),
    selectOperadoras(Goal),
    depthf2([],Nodo,Goal, L),
    retract(opList(List)).
%-----------------------------------------------------------------
% Excluir Operadoras Depth-First (3)
%-----------------------------------------------------------------

depthf3(P,Goal,Goal,[Goal|P]):- !, true.
depthf3(Path,Nodo,Goal,Sol) :-
    aresta(Nodo, ProxNodo),
    avoidOperadoras(ProxNodo),
    not(member(ProxNodo, Path)),
	depthf3([Nodo|Path],ProxNodo,Goal,Sol).	



solve_df_avoid(Nodo,Goal,List,L) :-
    paragem(Nodo,_,_,_,_,_,_,_,_,_,_),
    
    assert(opList(List)),
    avoidOperadoras(Nodo),
    avoidOperadoras(Goal),
    depthf3([],Nodo,Goal, L),
    retract(opList(List)).

%-----------------------------------------------------------------
% Obter paragem com mais carreiras Depth-First (4)
%-----------------------------------------------------------------
depthf4(P,Goal,Goal,[Goal|P],H,H).
depthf4(Path,Nodo,Goal,Sol,H,Hn) :-
    aresta(Nodo, ProxNodo),
    not(member(ProxNodo, Path)),
    paragem(X,_,_,_,_,_,_,Ops,_,_,_),
    length(Ops,  In),Int is -In,
    add_to_heap(H,Int,ProxNodo,H2),
	depthf4([Nodo|Path],ProxNodo,Goal,Sol,H2,Hn).	

%solve_df(11, 23, L).
solve_df_maxCarreiras(Nodo,Goal,N,L,T) :-
    paragem(Nodo,_,_,_,_,_,_,Ops,_,_,_),
    empty_heap(H),
    length(Ops, In),Int is -In,
    add_to_heap(H,Int,Nodo,H2),
    depthf4([],Nodo,Goal, L,H2,Hn),!,

    heap_size(Hn, Size),
    min_member( Max,[Size,N]),
    truncateHeap(Hn,Max,T).

/***
 *  Breadth-first List Simples
*/
solve_bf( Start,Goal, Solution) :-
    breadthf( [[Start]],Goal,Solution).
breadthf([[Node | Path]|_],Node,[Node | Path] ).

breadthf([[N | Path]| Paths],Goal, Solution) :-
    findall([M,N|Path],( aresta(N,M), not(member(M,[N|Path]))),NewPaths),
    append( Paths, NewPaths, Paths1), !,
    breadthf(Paths1, Goal,Solution);breadthf(Paths,Goal,Solution).
%findall(C, solve_bf(11,23,C) , B).
/**
 * Breadth First List Limitado Simples
**/
solve_bf_lim( Start,Goal, Solution) :-
    breadthfl( [[Start]],Goal,Solution).
breadthfl([[Node | Path]|_],Node,[Node | Path] ).

breadthfl([[N | Path]| Paths],Goal, Solution) :-
    length(Path,L),L=< 50,
    findall([M,N|Path],( aresta(N,M), not(member(M,[N|Path]))),NewPaths),
    append( Paths, NewPaths, Paths1), !,
    breadthfl(Paths1, Goal,Solution);breadthfl(Paths,Goal,Solution).

/**
*   Breadth First tree Com Functor
**/
solve_bft_engine( Start,Finish,Test,Solution) :-bft( l(Start),Finish,Test,Solution).
bft( Tree,Goal,Test,Solution) :-
    expand([],Tree,Goal,Test,Treel, Solved, Solution),
    ( Solved= yes;
      Solved= no, bft(Treel,Goal,Test,Solution) ).

expand(P,l(N),N,_,_,yes,[N|P]).

expand(P,l(N),_,Test,t(N,Subs),no,_) :-

    findall( l(M), 
    ( aresta(N,M), not(member(M,P)),append(Test,[M],Builder),F=..Builder,F),
     Ret),sort(Ret,Subs).

expand( P, t(N,Subs),Goal,Test, t( N, Subsl), Solved, Sol) :-
    expandall([N|P],Goal,Test,Subs, [], Subsl, Solved, Sol).

expandall(_,_,_,[], [T|Ts], [T|Ts],no,_).
expandall( P,Goal,Test,[T|Ts], Ts1, Subs1, Solved, Sol) :-
    expand(P,T,Goal,Test,T1,Solved1,Sol),
    (Solved1 =yes, Solved = yes;
     Solved1 =no,!,expandall( P,Goal,Test,Ts, [T1|Ts1], Subs1, Solved, Sol));
    expandall(P,Goal,Test,Ts,Ts1,Subs1,Solved,Sol).
/**
*   Breadth First tree simples
**/
solve_bft( Start,Finish,Solution) :-
    true=..Test,
    solve_bft_engine( Start,Finish,Test,Solution).
/**
*   Bft apenas operadoras selecionadas (2)
**/
solve_bft_select(Start, Finish,List,Sol):-
    assert(opList(List)),
    selectOperadoras=..Test,
    solve_bft_engine( Start,Finish,Test,Solution),
    retract(opList(List)).
/**
*   Bft com operadoras excluidas (3)
**/
solve_bft_avoid(Start, Finish,List,Sol):-
    assert(opList(List)),
    avoidOperadoras=..Test,
    solve_bft_engine( Start,Finish,Test,Solution).
/**
*   Bft Apenas Publicidade
**/
solve_bft_pub(Start, Finish,Sol):-
    hasPublicidade=..Test,
    solve_bft_engine( Start,Finish,Test,Solution).
/**
*   Bft Apenas Abrigado
**/
solve_bft_abrigado(Start, Finish,Sol):-
    isAbrigado=..Test,
    solve_bft_engine( Start,Finish,Test,Solution).
/**
 * Bft CheckPoints 
**/
solve_bft_chk( Start,Finish,List,Solution) :-
    true=..Test,
    assert(cp(List)),
    bft2( l(Start),Finish,Test,Solution),
    retract(cp(List)).
bft2( Tree,Finish,Test,Solution) :-
    expand([],Tree,Finish,Test,Treel, Solved, Solution),
    ( Solved= yes, (write(Solution),checksPoints(Solution); bft2(Treel,Finish,Test,Solution));
      Solved= no, bft2(Treel,Finish,Test,Solution)).
%solve_bft_chk(110,287,[258,200],S)
%solve_bft(110,200,S)
solve_bft_limited( Start,Finish,Solution) :-
    true=..Test,
    bft( l(Start),Finish,Test,Solution,500).
bftl( Tree,Goal,Test,Solution,Lim) :-
    expand([],Tree,Goal,Test,Treel, Solved, Solution), Nlim=Lim-1,
    Nlim>0,
    ( Solved= yes; 
      Solved= no, bftl(Treel,Goal,Test,Solution,Nlim) );
    bftl(Treel,Goal,Test,Solution,Nlim).
%solve_bft_limited( 110,433,Solution).
    