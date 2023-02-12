:- use_module(rob_utils).
:- use_module(func_utils).

test(A,B):-
   A~>plus(50)~>numlist_(1)~>
   fold_(plus,10)~>
   n_factorial~>
   number_digits~>fold_(plus,0)~>B.
