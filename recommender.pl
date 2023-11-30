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

/* [ingredient_substitute/2] */
ask_for_substitute(Ingredient) :-
    format('What can be used as a substitute for ~w? (Type "none" if there is no substitute): ', [Ingredient]),
    read_line_to_string(user_input, Substitute),
    (Substitute \= "none" -> assertz(ingredient_substitute(Substitute, Ingredient)); true).

/* [excluded_ingredients] */
ask_for_excluded_ingredients :-
    format('Is there any you cannot eat? (type "done" when finished): '),
    read_line_to_string(user_input, Ingredient),
    (Ingredient \= "done" -> assertz(excluded_ingredient(Ingredient)), ask_for_excluded_ingredients; true).

/* [DATA STRUCTURE OPS] */
equivalent_lists(List1, List2) :-
    sort(List1, Sorted),
    sort(List2, Sorted).

/* [DATA LOADING AND PARSING] */
load_recipes :-
    write('Loading recipes... it will takes some time.'), nl,
    retractall(recipe(_, _, _, _)), % Clear existing recipes
    csv_read_file('recipes.csv', Rows, []),
    maplist(assert_recipe, Rows),
    write('Recipes loaded successfully.').
    
atom_to_list(Atom, List) :-
    read_term_from_atom(Atom, List, []).

assert_recipe(Row) :-
    Row = row(Name, IngredientsAtom, StepsAtom, Description),
    atom_to_list(IngredientsAtom, Ingredients), % string to List
    atom_to_list(StepsAtom, Steps),
    assertz(recipe(Name, Ingredients, Steps, Description)).
/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* [Main function] */
% Predicate to recommend recipes based on available ingredients
recommend_recipes(AvailableIngredients) :-
    ask_for_excluded_ingredients,
    findall(Recipe, (
        recipe(Recipe, RecipeIngredients, _, _),
        subset(RecipeIngredients, AvailableIngredients),
        not_contains_excluded_ingredients(RecipeIngredients)
    ), CompleteMatches),
    findall((Recipe, MissingIngredients), (
        recipe(Recipe, RecipeIngredients, _, _),
        subset(AvailableIngredients, RecipeIngredients),
        \+ equivalent_lists(AvailableIngredients, RecipeIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        find_missing_ingredients(AvailableIngredients, RecipeIngredients, MissingIngredients)
    ), PartialMatches),
    print_complete_matches(CompleteMatches),
    print_partial_matches(PartialMatches).
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

print_list_bold([]).
print_list_bold([Head|Tail]) :-
    format('* ~c[1m~w~c[0m\n', [27,Head,27]),
    print_list_bold(Tail).

% Print complete matches
print_complete_matches([]) :-
    format('~n[No recipes found with all ingredients available]           \n',[]).

print_complete_matches(CompleteMatches) :-
    format('~n[Recipes with all ingredients ~c[34mavailable~c[0m]                \n',[27,27]),
    print_list_bold(CompleteMatches).

% Print partial matches
print_partial_matches([]) :-
    format('~n[No recipes found with partial ingredients ~c[34mavailable~c[0m]          \n',[27,27]).

print_partial_matches(PartialMatches) :-
    format('~n[Recipes with some ingredients ~c[31mmissing~c[0m]                 \n',[27,27]),
    maplist(print_partial_match, PartialMatches).

print_partial_match((Recipe, MissingIngredients)) :-
    format('* ~c[1m~w~c[0m\nMissing ingredients: ~w~n\n', [27,Recipe,27,MissingIngredients]).

/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

/* [Recipe handling functions] */
recipe_details(Name) :-
    recipe(Name, Ingredients, Steps, Description),
    format('Recipe for ~c[1m~w~c[0m:\n\n', [27, Name, 27]),
    format('~c[1mDescription~c[0m:\n~w\n\n', [27,27,Description]),
    format('~c[1mIngredients~c[0m:\n~w\n\n', [27,27,Ingredients]),
    format('~c[1mSteps~c[0m:\n\n', [27,27]),
    print_list(Steps).

% Helper predicate to ensure the recipe does not contain excluded ingredients
not_contains_excluded_ingredients(RecipeIngredients) :-
    \+ (member(Ingredient, RecipeIngredients), excluded_ingredient(Ingredient)).

% Helper predicate to find missing ingredients
find_missing_ingredients(Available, RecipeIngredients, Missing) :-
    subtract(RecipeIngredients, Available, Missing).






/*========================================================================
   Start Interface.
========================================================================*/

info:-
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27, 27]),
   format('~n>~c[1m Commis: food recommdender by Jaehwan Lee and Insun Baek             ~c[0m<',[27, 27]),
   format('~n>                                                                     <',[]),
   format('~n> ?- ~c[32mload_recipes.~c[0m      - Load recipes from storage                   <',[27,27]),
   format('~n> ?- ~c[32mrecommend_recipes.~c[0m - Ask Commis with available ingredients       <',[27,27]),
   format('~n> ?- ~c[32mrecipe_details.~c[0m    - Commis will describe recipe in detail       <',[27,27]),
   format('~n>                                                                     <',[]),
   format('~n> (Example)                                                           <',[]),
   format('~n> ?- load_recipes.                                                    <',[]),
   format('~n> ?- recommend_recipes([\'sugar\', \'salt\', \'pork\', \'oil\']).             <',[]),
   format('~n> ?- recipe_details(\'pork strips\').                                   <',[]),
   format('~n>                                                                     <',[]),
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27,27]),
   format('~n~n',[]).

:- initialization(info).