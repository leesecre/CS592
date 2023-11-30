% Define the extraction predicate

get_functor(Predicate, Functor) :-
    functor(Predicate, Functor, _).

extract_constants(egg(_), Constants) :-
    Constants = ['egg'].

extract_constants(milk(_), Constants) :-
    Constants = ['milk'].

extract_constants(not(A), Constants) :-
    extract_constants(A, Constants).

extract_constants(some(_, and(B, C)), Constants) :-
    extract_constants(B, BConstants),
    extract_constants(C, CConstants),
    append(BConstants, CConstants, Constants).

extract_constants(and(A, B), Constants) :-
    extract_constants(A, AConstants),
    extract_constants(B, BConstants),
    append(AConstants, BConstants, Constants).

extract_constants(eat(_, _), Constants) :-
    Constants = [].

extract_constants(with(_, _), Constants) :-
    Constants = [].

extract_constants(recipe(_), Constants) :-
    Constants = [].

extract_constants(want(_, _), Constants) :-
    Constants = [].

% Example usage:
% ?- extract_constants(some(A, and(and(some(B, and(and(egg(B), milk(B)), with(A, B))), recipe(A)), want(mia, A))), Result).
% Result = [egg, milk].
