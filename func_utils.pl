%% Contains my versions of functional predicates like scan and fold
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


