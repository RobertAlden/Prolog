%% Contains my versions of functional predicates like scan and fold
:- module(func_utils,  [scan_fold_/5,
						scan_fold_r/5,
						scan_/4,
						scan_r/4,
						fold_/4,
						fold_r/4,
						reduce/3,
						scan_/3,
						power/4,
						commute/3,
						rotate/3,
						op(675, xfy, (~>)),
						(~>)/2
					   ]).
:- use_module(rob_utils, [reverse_/2]).

scan_fold_(_, Acc, Acc, [], []).
scan_fold_(Goal, Acc, R, [I|Li], [AccN|Lo]):-
	call(Goal, Acc, I, AccN),
	scan_fold_(Goal, AccN, R, Li, Lo).

scan_fold_r(Goal, Acc, O, Li, Lo):-
	reverse_(Li,LiR),
	scan_fold_(Goal, Acc, O, LiR, Lo).
	
scan_(Goal, Acc, Li, Lo) :- scan_fold_(Goal, Acc, _, Li, Lo).
fold_(Goal, Acc, Li, R) :- scan_fold_(Goal, Acc, R, Li, _).
scan_r(Goal, Acc, Li, Lo) :- scan_fold_r(Goal, Acc, _, Li, Lo).
fold_r(Goal, Acc, Li, R) :- scan_fold_r(Goal, Acc, R, Li, _).

reduce(+,Li,Lo) :- fold_(plus,0,Li,Lo).
reduce(*,Li,Lo) :- fold_([X,Y,Z]>>(X*Y#=Z),1,Li,Lo).
scan_(+,Li,Lo) :- scan_(plus,0,Li,Lo).
scan_(*,Li,Lo) :- scan_([X,Y,Z]>>(X*Y#=Z),1,Li,Lo).

power(0,_,R,R).
power(N,Goal,I,O):-
	N #> 0,
	Nn #= N - 1,
	call(Goal,I,X),
	power(Nn,Goal,X,O).

commute(Goal,A,B) :-
	call(Goal,B,A).

rotate(R,Li,Rl):-
	length(Li,X),
	length(Rl,X),
	N #= R mod X,
	NX #= -X,
	N in NX..X,
	labeling([],[N]),
	zcompare(C,N,0),
	rotate(C,N,Li,Rl).
rotate(>,N,Li,Rl) :-
	length(Fr,N),
	append(Fr,Bck,Li),
	append(Bck,Fr,Rl).
rotate(=,0,Li,Li).
rotate(<,N,Li,Rl) :-
	length(Li,X),
	length(Rl,X),
	Y #= X + N,
	zcompare(C,Y,0),
	rotate(C,Y,Li,Rl).





:- meta_predicate ~>(+,+).
A ~> B :- 
	once(process_term(A~>B)).

xfy_list_(_, Term, [Term]):- var(Term).
xfy_list_(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list_(Op, Right, List),
    !.

process_term(Term) :- 
	xfy_list_(~>,Term,L),
	append([Head_term],L0,L),
	append(L1,[Last_term],L0),
	thread_state(L1, Goals, Head_term, Last_term),
	once(execute_goals(Goals)).

thread_state([], [], Out, Out).
thread_state([F|Funcs], [Goal|Goals], In, Out) :-
    F =.. [Functor|Args],
    append(Args, [In, Tmp], NewArgs),
    Goal =.. [Functor|NewArgs],
    thread_state(Funcs, Goals, Tmp, Out).

execute_goals([Goal]) :- 
	call(Goal).
execute_goals([Goal|Goals]) :-
	call(Goal),
	execute_goals(Goals).
