:- use_module(rob_utils).
:- use_module(func_utils).

fib(N, F):- 
    fib(N, 1, 0, F).
fib(1,R,_,R).
fib(N, A, B, R):-
    N #> 1,
    Next #= N-1,
    AB #= A+B,
    fib(Next, AB, A, R).

fib_sequence(First, Last, Seq) :-
    findall(F, (between(First,Last,N), fib(N,F)), Seq).

even(N):- N mod 2 #= 0.

answer:-
    60~>fib_sequence(1)~>
    include([X]>>(X<4000000))~>include(even)~>
    reduce(+)~>Result,
    writeln(Result).
