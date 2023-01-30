:- use_module(rob_utils, [fetch_file/2, windowed/3, digits_/2]).

mult(X,Y,Z) :- Z #= X*Y.
prod_l(X,P) :- foldl(mult,X,1,P).

pe8(R):-
    fetch_file('ProjectEuler\\PE0008\\pe8.txt',Contents),
    foldl(string_concat,Contents,'',N),
    atom_number(N,X),
    digits_(X,Ds),
    windowed(Ds,13,Ws),
    maplist(prod_l,Ws,Ps),
    max_list(Ps,R).

