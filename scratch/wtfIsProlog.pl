%sum_range(A, B, Result) :- true if sum of all ints in range [A,B] is
% Result
% base case
sum_range(A,A,A).

%recursive case
sum_range(A,B,Result):-
    A1 is A+1,
    sum_range(A1,B,TempResult),
    Result is A + TempResult.

fib(0,0).
fib(1,1).
fib(N, R) :-
    N>1,
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1), fib(N2,R2),
    R is R1 + R2.


factorial(0, 1).
factorial(N, F):-
    N>0,
    N1 is N-1,
    factorial(N1,F1),
    F is F1 * N.

factorial_it(N,F):-
    factorial_it(0,N,1,F).

factorial_it(N,N,F,F).
factorial_it(I,N,T,F):-
    I < N,
    I1 is I+1,
    T1 is T * I1,
    factorial_it(I1,N,T1,F).





