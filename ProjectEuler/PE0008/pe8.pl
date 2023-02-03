:- use_module(rob_utils, [fetch_file/2, list_reverse/2, windowed/3, digits_/2]).

mult(X,Y,Z) :- Z #= X*Y.
prod_l(X,P) :- foldl(mult,X,1,P).
foldr(Goal,L1,X,L2) :- 
    list_reverse(L1,L1rev),
    foldl(Goal,L1rev,X,L2). 

pe8(R):-
    fetch_file('ProjectEuler\\PE0008\\pe8.txt',Contents),
    foldr(string_concat,Contents,'',N),
    atom_number(N,X),
    digits_(X,Ds),
    windowed(Ds,13,Ws),
    maplist(prod_l,Ws,Ps),
    max_list(Ps,R).

