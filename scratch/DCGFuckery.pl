:-use_module(library(pio)).

all([])     --> [].
all([L|Ls]) --> [L], all(Ls).

fetch_file(F, R):-
    once(phrase_from_file(all(Ls), F)),
    maplist([X,Y]>>(char_code(Y,X)),Ls,C), % this is wild.
    string_chars(Cs,C), % what string Cs relates to the list of chars C? Prolog knows!
    split_string(Cs, '\n', '\s\t', R). % split that string on \n, removing \s\t padding from the resulting substrings.

