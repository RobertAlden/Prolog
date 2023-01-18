:-use_module(library(pio)).

all([])     --> [].
all([L|Ls]) --> [L], all(Ls).

fetch_file(F, R):-
    once(phrase_from_file(all(Ls), F)),
    maplist([X,Y]>>(char_code(Y,X)),Ls,C),
    string_chars(Cs,C),
    split_string(Cs, '\n', '\s\t', R).

