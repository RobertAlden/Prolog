
pe1(Z,Sum):-
    findall(N, ([X]>>(N mod X #= 0), N in 0..Z,
                   indomain(N)),
               Ns),
       sum(Ns, #=, Sum).
