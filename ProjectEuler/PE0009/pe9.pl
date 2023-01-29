answer:-
    chain([A,B,C], #<),
    (A*A) + (B*B) #= (C*C),
    A + B + C #= 1000,
    [A,B,C] ins 1..1000,
    once(labeling([down],[A,B,C])),
    X #= A*B*C,
    writeln(X).