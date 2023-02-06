%% Contains my versions of functional predicates like scan and fold
:- module(func_utils,  [scan_fold_/5,
						scan_fold_r/5,
						scan_/4,
						scan_r/4,
						fold_/4,
						fold_r/4,
						power/4,
						commute/3]).
:- use_module(rob_utils, [list_reverse/2]).

scan_fold_(_, Acc, Acc, [], []).
scan_fold_(Goal, Acc, R, [I|Li], [AccN|Lo]):-
	call(Goal, Acc, I, AccN),
	scan_fold_(Goal, AccN, R, Li, Lo).

scan_fold_r(Goal, Acc, O, Li, Lo):-
	list_reverse(Li,LiR),
	scan_fold_(Goal, Acc, O, LiR, Lo).
	
scan_(Goal, Acc, Li, Lo) :- scan_fold_(Goal, Acc, _, Li, Lo).
fold_(Goal, Acc, Li, R) :- scan_fold_(Goal, Acc, R, Li, _).
scan_r(Goal, Acc, Li, Lo) :- scan_fold_r(Goal, Acc, _, Li, Lo).
fold_r(Goal, Acc, Li, R) :- scan_fold_r(Goal, Acc, R, Li, _).

power(0,_,R,R).
power(N,Goal,I,O):-
	N #> 0,
	Nn #= N - 1,
	call(Goal,I,X),
	power(Nn,Goal,X,O).

commute(Goal,A,B) :-
	call(Goal,B,A).