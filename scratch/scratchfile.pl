
reverse_([]) --> [].
reverse_([L|Ls]) --> reverse_(Ls), [L].

fib_topdown_list_cache_clpfd(N,F,Cache) :-
   M is N+1,
   length(Cache,M),
   setup_constraints(Cache),
   Cache=[F|_].

setup_constraints([0]).
setup_constraints([1,0]).
setup_constraints([F,FA,FB|More]) :-
   F #= FA+FB,
   setup_constraints([FA,FB|More]).