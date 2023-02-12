:- use_module(rob_utils).
:- use_module(func_utils).

pe8(R):-
    'ProjectEuler\\PE0008\\pe8.txt'~>fetch_file~>
    fold_(string_concat,'')~>atom_number~>number_digits~>
    windowed(13)~>maplist(reduce(*))~>max_list~>R.

