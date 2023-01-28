

parse_stuff(String, term(Coeff, Pow)) :-
    string_concat(CoeffStr, MonomialStr, String),
    string_concat("x^", PowStr, MonomialStr),
    number_string(Coeff, CoeffStr),
    number_string(Pow, PowStr).