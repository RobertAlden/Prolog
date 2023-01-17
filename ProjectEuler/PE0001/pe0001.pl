three(N):- N mod 3 =:= 0.
five(N):- N mod 5 =:= 0.

answer:-
    numlist(1,999,R),
    include(three, R, Threes),
    include(five, R, Fives),
    union(Threes, Fives, ThreesAndFives),
    sum_list(ThreesAndFives, Result),
    writeln(Result).
