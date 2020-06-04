s(0):-loadGraph(1).
s(1):-
    loadGraph(1),
    solve_df(11,23,Caminho),
    write(Caminho),
    unloadGraph(1).

s(2):-
    loadGraph(1),
    solve_df_select(11,23,["Carris","Vimeca"],Caminho),
    write(Caminho),
    unloadGraph(1).
s(3):-
    solve_df_select(11,23,["Vimeca"],Caminho),
    write(Caminho).
s(4):-
    solve_df_avoid(11,23,["Carris"],Caminho),
    write(Caminho).
s(5):-
    solve_df_maxCarreiras(11,23,2,L,Carrs),
    write(L),write('\n'),
    write(Carrs) .
s(6):-
    solve_bft_select(11, 23,["Carris"],Sol),
    write(Sol).