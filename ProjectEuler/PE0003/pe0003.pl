allPrimes(N, Primes) :-
    numlist(2, N, Numlist),
    Stop is round(sqrt(N)),
    allPrimes(Numlist, Stop, Primes).
    
allPrimes([N|Numlist], Stop, [N|Primes]):-
  exclude(is_multiple(N), Numlist, MPrimes),
  (N =< Stop -> allPrimes(MPrimes, Stop, Primes) ; Primes=MPrimes).
  
is_multiple(N, I):-
  I mod N =:= 0.

is_divisible(N, I):-
  N mod I =:= 0.

answer:-
    Number is 600851475143,
    Root is round(sqrt(Number)),
    allPrimes(Root,NPrimes),
    include(is_divisible(Number), NPrimes, Factors),
    max_list(Factors, Result),
    writeln(Result).
