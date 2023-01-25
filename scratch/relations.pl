person_age(bill, 32).
person_age(robert, 28).
person_age(jane, 65).
person_age(sally, 53).
person_age(jack, 18).
person_age(mike, 22).
person_age(jimmy, 34).
person_age(blorpulon_the_infinite, 329839311929391284244328947283).
person_age(larry, 53).
person_age(barbara, 31).

is_older_than(X,Y) :-
	person_age(X,Xa),
	person_age(Y,Ya),
	Xa #> Ya.

is_age_even(X) :-
	person_age(X,Xa),
	Xa mod 2 #= 0 #\/ Xa #= 53.