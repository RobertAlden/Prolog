


fib_it(N,R):-
    fib_it(N,1,0,R).

fib_it(1,R,_,R).
fib_it(N,F1,F2,R):-
    N>1,
    fib_it((N-1),(F1+F2),F1,R).
