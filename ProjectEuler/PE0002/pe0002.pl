fib(N, F):- 
    fib(N, 1, 0, F).
fib(1,R,_,R).
fib(N, A, B, R):-
    N > 1,
    Next is N-1,
    AB is A+B,
    fib(Next, AB, A, R).

fib_sequence(First, Last, Seq) :-
    findall(F, (between(First,Last,N), fib(N,F)), Seq).

even(N):- N mod 2 =:= 0.

answer:-
    fib_sequence(1,60,FibSeq),
    include([X]>>(X<4000000),FibSeq,FibLessThen4M),
    include(even, FibLessThen4M, Final),
    sum_list(Final, Result),
    writeln(Result).
