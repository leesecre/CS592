logical_expression_to_list(egg(Atom), ['egg']).
logical_expression_to_list(milk(Atom), ['milk']).
logical_expression_to_list(burger(Atom), ['burger']).

logical_expression_to_list(with(A,B), List).
logical_expression_to_list(recipe(A), List).
logical_expression_to_list(want(A, B), List).

logical_expression_to_list(and(Expression1, Expression2), List) :-
    logical_expression_to_list(Expression1, List1),
    logical_expression_to_list(Expression2, List2),
    append(List1, List2, List).

logical_expression_to_list(some(Variable, Expression), List) :-
    logical_expression_to_list(Expression, List).
