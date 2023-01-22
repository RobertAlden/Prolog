:- use_module(rob_utils).

test(O) :- numlist_(1,10,L),
           tfilter(is_odd,L,O). 