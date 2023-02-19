:- use_module(rob_utils).
:- use_module(func_utils).
:- use_module(prime_utils).

:- table t/2.


t(N, [N]) :- is_prime(N).
t(N, [N,TA,TB]) :-
    N #= A * B,
    \+ is_prime(N),
    isqrt(N,Lim),
    A in 1..Lim,
    A #=< B,
    once(labeling([down], [A,B])),
    t(A, TA),
    t(B, TB).


trees_match([_,AL,AR], [_,BL,BR]) :-
    trees_match(AL,BL),
    trees_match(AR,BR).
trees_match([_], [_]).


m(N,K) :-
    df(N,Ndf), 
    K in 2..Ndf,
    t(Ndf,TN),
    labeling([min(K)],[K]),
    t(K,TK),
    trees_match(TN,TK).

df(N,F) :-
    if_(mod_(N,2), step_range(2,2,N,R), step_range(2,3,N,R)),
    reduce(*,R,F).

pe829(A,B,R) :-
    range(A,B)~>
    maplist(df)~>
    maplist(m)~>
    reduce(+)~>R.    

answer:-
    pe829(2,31,R),
    writeln(R).

test(X,R) :-
    findall(Y, gen(X,Y))~>min_list~>R.

    
gen(Lim,R) :-
    df(Lim,Ldf),
    ground(Lim),
    prime_factors(Ldf,Fs),
    t(Ldf,LdfT),
    length(Fs,Len),
    length(L0,Len),
    L0 ins 2..Lim,
    reduce(*,L0,R),
    R #=< Ldf,
    ascending_(L0),
    maplist([X]>>(nth0(_,[2,3,5,7,11,13,17,19,23,29,31],X)),L0),
    %writeln(Ldf),
    %writeln(R),
    t(R,RT),
    trees_match(LdfT,RT),
    m(Lim,R),
    %writeln(L0),
    labeling([enum],L0).
