/* [GLOBAL DEFINE] */
:- dynamic recipe/4.
:- dynamic available_ingredient/1.
:- dynamic ingredient_substitute/2.
:- dynamic excluded_ingredient/1.

/* [available_ingredient/1] */
add_available_ingredient(Ingredient) :- % Predicate to add an available ingredient
    assertz(available_ingredient(Ingredient)).

clear_available_ingredients :- % Predicate to clear all available ingredients
    retractall(available_ingredient(_)).

list_available_ingredients(List) :-
    findall(Ingredient, available_ingredient(Ingredient), List).

/* [ingredient_substitute/2] */
ask_for_substitute(Ingredient) :-
    format('What can be used as a substitute for ~w? (Type "none" if there is no substitute): ', [Ingredient]),
    read_line_to_string(user_input, Substitute),
    (Substitute \= "none" -> assertz(ingredient_substitute(Substitute, Ingredient)); true).

/* [excluded_ingredients] */
ask_for_excluded_ingredients :-
    format('Is there any you cannot eat? (type "none" when finished): '),
    read_line_to_string(user_input, Ingredient),
    (Ingredient \= "none" -> assertz(excluded_ingredient(Ingredient)), ask_for_excluded_ingredients; true).

/* [DATA STRUCTURE OPS] */
equivalent_lists(List1, List2) :-
    sort(List1, Sorted),
    sort(List2, Sorted).

member_of_both(ListA, ListB) :-
    member(Element, ListA),
    member(Element, ListB),
    !. 

/* [DATA LOADING AND PARSING] */
load_all :-
    write('Loading previous data...'), nl,
    retractall(available_ingredient(_)),
    retractall(ingredient_substitute(_)),
    retractall(excluded_ingredient(_)),
    load_from_csv('user_data.csv'),
    write('Loading recipes... it will takes some time.'), nl,
    retractall(recipe(_, _, _, _)), % Clear existing recipes
    csv_read_file('recipes.csv', Rows, []),
    maplist(assert_recipe, Rows),
    write('Recipes loaded successfully.').

save_all :-
    save_to_csv('user_data.csv').
    
atom_to_list(Atom, List) :-
    read_term_from_atom(Atom, List, []).

assert_recipe(Row) :-
    Row = row(Name, IngredientsAtom, StepsAtom, Description),
    atom_to_list(IngredientsAtom, Ingredients), % string to List
    atom_to_list(StepsAtom, Steps),
    assertz(recipe(Name, Ingredients, Steps, Description)).

/* User data loading */
load_from_csv(FileName) :-
    open(FileName, read, Stream),
    read_file(Stream),
    close(Stream).

read_file(Stream) :-
    read_line_to_string(Stream, Line),
    Line \= end_of_file,
    split_string(Line, ",", "", [Predicate|Args]),
    assert_fact(Predicate, Args),
    read_file(Stream).
read_file(_).

assert_fact('available_ingredient', [Ingredient]) :-
    assertz(available_ingredient(Ingredient)),
    format('available ingredients loaded.\n').

assert_fact('ingredient_substitute', [Ingredient, Substitute]) :-
    assertz(ingredient_substitute(Ingredient, Substitute)),
    format('substitution information loaded.\n').

assert_fact('excluded_ingredient', [Ingredient]) :-
    assertz(excluded_ingredient(Ingredient)),
    format('Excluded ingredients loaded.\n').

/* User data saving */
save_to_csv(FileName) :-
    open(FileName, write, Stream),
    write_ingredients(Stream),
    write_substitutes(Stream),
    write_exclusions(Stream),
    close(Stream).

write_ingredients(Stream) :-
    available_ingredient(Ingredient),
    write(Stream, 'available_ingredient,'), 
    write(Stream, Ingredient), 
    write(Stream, '\n'),
    fail.
write_ingredients(_).

write_substitutes(Stream) :-
    ingredient_substitute(Ingredient, Substitute),
    write(Stream, 'ingredient_substitute,'), 
    write(Stream, Ingredient), 
    write(Stream, ','), 
    write(Stream, Substitute), 
    write(Stream, '\n'),
    fail.
write_substitutes(_).

write_exclusions(Stream) :-
    excluded_ingredient(Ingredient),
    write(Stream, 'excluded_ingredient,'), 
    write(Stream, Ingredient), 
    write(Stream, '\n'),
    fail.
write_exclusions(_).
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* [Main function] */
% Predicate to recommend recipes based on available ingredients
recommend_recipes() :-
    list_available_ingredients(AvailableIngredients),
    findall(Recipe, (
        recipe(Recipe, RecipeIngredients, _, _),
        subset(RecipeIngredients, AvailableIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 2  % To avoid abnormal data
    ), CompleteMatches),
    findall((Recipe, MissingIngredients), (
        recipe(Recipe, RecipeIngredients, _, _),
        member_of_both(AvailableIngredients, RecipeIngredients),
        \+ equivalent_lists(AvailableIngredients, RecipeIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        find_missing_ingredients(AvailableIngredients, RecipeIngredients, MissingIngredients),
        length(MissingIngredients, Length),
        Length < 2
    ), PartialMatches),
    print_complete_matches(CompleteMatches),
    format('~n~c[31m[Recipes with some ingredients missing]~c[0m                 \n',[27,27]),
    print_partial_matches(PartialMatches, 20).

% Predicate to recommend recipes based on available ingredients
recommend_recipes(AvailableIngredients) :-
    findall(Recipe, (
        recipe(Recipe, RecipeIngredients, _, _),
        subset(RecipeIngredients, AvailableIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 2  % To avoid abnormal data
    ), CompleteMatches),
    findall((Recipe, MissingIngredients), (
        recipe(Recipe, RecipeIngredients, _, _),
        member_of_both(AvailableIngredients, RecipeIngredients),
        \+ equivalent_lists(AvailableIngredients, RecipeIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        find_missing_ingredients(AvailableIngredients, RecipeIngredients, MissingIngredients),
        length(MissingIngredients, Length),
        Length < 2
    ), PartialMatches),
    print_complete_matches(CompleteMatches),
    format('~n~c[31m[Recipes with some ingredients missing]~c[0m                 \n',[27,27]),
    print_partial_matches(PartialMatches, 20).
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* [PRINT RELATED FUNCTIONS] */
print_bold(Text) :-
    format('~c[1m~w~c[0m', [27, Text, 27]).

print_red(Text) :-
    format('~c[31m~w~c[0m', [27, Text, 27]).

print_green(Text) :-
    format('~c[32m~w~c[0m', [27, Text, 27]).

print_blue(Text) :-
    format('~c[34m~w~c[0m', [27, Text, 27]).

% Predicate to print each element of a list on a new line.
print_list([]).
print_list([Head|Tail]) :-
    format('* ~w~n\n', [Head]),
    print_list(Tail).

print_list_skip(List) :-
    write('['),
    length(List, Length),
    (   Length > 10
    ->  print_first_cut(List, 10)
    ;   print_all(List) 
    ),
    write(']\n\n').

print_first_cut(_, 0) :- 
    write('...').
print_first_cut([H|T], N) :-
    N > 0,
    write(H), write(', '),
    N1 is N - 1,
    print_first_cut(T, N1).

print_all([]).
print_all([H|T]) :-
    write(H), write(', '),
    print_all(T).

print_list_bold([]).
print_list_bold([Head|Tail]) :-
    format('* ~c[1m~w~c[0m\n', [27,Head,27]),
    print_list_bold(Tail).

% Print complete matches
print_complete_matches([]) :-
    format('~n[No recipes found with all ingredients available]           \n',[]).

print_complete_matches(CompleteMatches) :-
    format('~n~c[34m[Recipes with all ingredients available]~c[0m                \n',[27,27]),
    print_list_bold(CompleteMatches).

print_partial_matches([], _) :-
    format('~n[No recipes found with partial ingredients ~c[34mavailable~c[0m]          \n',[27,27]).
print_partial_matches(_, 0). % Stop printing after 10 matches
print_partial_matches([(Recipe, MissingIngredients) | Rest], Counter) :-
    Counter > 0,
    format('* ~c[1m~w~c[0m\nMissing ingredients: ', [27,Recipe,27]),
    print_list_skip(MissingIngredients),
    NewCounter is Counter - 1,
    print_partial_matches(Rest, NewCounter).



% Predicate to print ingredients, highlighting missing ones in red
print_ingredients([]).
print_ingredients([Ingredient|Rest]) :-
    (   available_ingredient(Ingredient)
    ->  format('~w, ', [Ingredient])
    ;   print_red(Ingredient), format(', ')
    ),
    print_ingredients(Rest).

/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* [Recipe handling functions] */
recipe_details(Name) :-
    recipe(Name, Ingredients, Steps, Description),
    format('\nRecipe for ~c[1m~w~c[0m:\n\n', [27, Name, 27]),
    format('~c[1mDescription~c[0m:\n~w\n\n', [27,27,Description]),
    format('~c[1mIngredients~c[0m~c[31m(missing)~c[0m\n[', [27,27,27,27]),
    print_ingredients(Ingredients),
    format(']\n\n', []),
    format('~c[1mSteps~c[0m:\n\n', [27,27]),
    print_list(Steps).

% Helper predicate to ensure the recipe does not contain excluded ingredients
not_contains_excluded_ingredients(RecipeIngredients) :-
    \+ (member(Ingredient, RecipeIngredients), excluded_ingredient(Ingredient)).

% Helper predicate to find missing ingredients
find_missing_ingredients(Available, RecipeIngredients, Missing) :-
    subtract(RecipeIngredients, Available, Missing).



/* USER INTERFACE */

add_available_ingredient:-
    format('~c[1mcommis:~c[0m Okay, What do you have?\n',[27,27]),
    read_line_to_string(user_input, Ingredient),
    assertz(available_ingredient(Ingredient)),
    list_available_ingredients(AvailableIngredients),
    format("Then now you have belows \n"),
    print_list(AvailableIngredients).

hey_commis :-
    format('~c[1mcommis:~c[0m Hello, do you need help? :)\n',[27,27]),
    format('    1.  (type "add") want to add available ingredients.\n'),
    format('    2.  (type "exc") tell me ingredients you can\'t eat.\n'),
    format('    3.  (type "tea") teach me substitute for ingredients.\n'),
    read_line_to_string(user_input, Command),
    (Command = "add" -> add_available_ingredient, !;
     Command = "exc" -> format('hi'), !;
     Command = "tea" -> format('hi'), !;
     format("I cannot understand what yor say"), false).

/*========================================================================
   Start Interface.
========================================================================*/

info:-
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27, 27]),
   format('~n>~c[1m Commis: food recommdender by Jaehwan Lee and Insun Baek             ~c[0m<',[27, 27]),
   format('~n>                                                                     <',[]),
   format('~n> ?- ~c[32mload_all.~c[0m      - Load recipes and user data from storage         <',[27,27]),
   format('~n> ?- ~c[32msave_all.~c[0m      - Save recipes and user data to storage           <',[27,27]),
   format('~n> ?- ~c[32mhey_commis.~c[0m    - Ask commis, interface to communicate            <',[27,27]),
   format('~n>                                                                     <',[]),
   format('~n> (Example)                                                           <',[]),
   format('~n> ?- load_all.                                                    <',[]),
   format('~n> ?- recommend_recipes([\'sugar\', \'salt\', \'pork\', \'onion\']).           <',[]),
   format('~n> ?- recipe_details(\'pork strips\').                                   <',[]),
   format('~n>                                                                     <',[]),
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27,27]),
   format('~n~n',[]).

:- initialization(info).