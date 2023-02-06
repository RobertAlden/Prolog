:- use_module(rob_utils, [fetch_file/2, windowed/3]).
:- use_module(func_utils, [fold_/4]).

max_window_product(G,P) :-
    maplist([X,Y]>>(windowed(X,4,Y)),G,GW),
    maplist(
        [X,Y]>>(maplist(
            fold_([A,B,C]>>(A*B#=C),1),X,Y)
        ),
    GW,GP),
    maplist(max_list,GP,MGP),
    max_list(MGP,P).

skew_matrix(M) :-
    length(M,Mly),
    maplist(length,M,Mlxs),
    fold_([A,B,C]>>(A*B#=C),1,Mlxs,PMx),
    PMx #= Mly^Mly.

pe11(R) :-
    fetch_file('ProjectEuler\\PE0011\\pe11.txt',Data),
    maplist([X,Z]>>(
        split_string(X," ", "", X1),
        maplist(number_string,Z,X1)),
    Data,Grid),
    transpose(Grid,GridT),
    max_window_product(Grid,PG),
    max_window_product(GridT,PGT),
    max_list([PG,PGT],R).