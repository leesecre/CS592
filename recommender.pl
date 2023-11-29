/* [GLOBAL DEFINE] */
:- dynamic recipe/4.

/* [DATA LOADING AND PARSING] 
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
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

% Predicate to recommend recipes based on available ingredients
recommend_recipes(AvailableIngredients) :-
    findall(Recipe, (
        recipe(Recipe, RecipeIngredients,_,_),
        subset(AvailableIngredients, RecipeIngredients)
    ), RecommendedRecipes),
    print_recommendations(RecommendedRecipes).

/* [PRINT RELATED FUNCTIONS] 
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */
print_bold(Text) :-
    format('~c[1m~w~c[0m', [27, Text, 27]).

% Predicate to print each element of a list on a new line.
print_list([]).
print_list([Head|Tail]) :-
    format('o. ~w~n\n', [Head]),
    print_list(Tail).

print_recommendations([]) :-
    write('No matching recipes found with the available ingredients.').
print_recommendations(Recipes) :-
    write('Recommended recipes: '), nl,
    print_recipes(Recipes).

print_recipes([]).
print_recipes([Recipe|Rest]) :-
    write('- '), write(Recipe), nl,
    print_recipes(Rest).

/* <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< */

recipe_details(Name) :-
    recipe(Name, _, Steps, Description),
    format('Recipe for ~c[1m~w~c[0m:\n\n', [27, Name, 27]),
    format('~c[1mDescription~c[0m:\n~w\n\n', [27,27,Description]),
    format('~c[1mSteps~c[0m:\n\n', [27,27]),
    print_list(Steps).

% substitute ingredients
:- dynamic ingredient_substitute/2.

ask_for_substitute(Ingredient) :-
    format('What can be used as a substitute for ~w? (Type "none" if there is no substitute): ', [Ingredient]),
    read_line_to_string(user_input, Substitute),
    (Substitute \= "none" -> assertz(ingredient_substitute(Substitute, Ingredient)); true).

/*========================================================================
   Info
========================================================================*/

info:-
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27, 27]),
   format('~n>~c[1m Commis: food recommdender by Jaehwan Lee and Insun Baek             ~c[0m<',[27, 27]),
   format('~n>                                                                     <',[]),
   format('~n> ?- load_recipes.      - Load recipes from storage                   <',[]),
   format('~n> ?- recommend_recipes. - Ask Commis with available ingredients       <',[]),
   format('~n> ?- recipe_details.    - Commis will describe recipe in detail       <',[]),
   format('~n>                                                                     <',[]),
   format('~n> (Example)                                                           <',[]),
   format('~n> ?- load_recipes.                                                    <',[]),
   format('~n> ?- recommend_recipes([\'sugar\', \'salt\', \'pork\']).                    <',[]),
   format('~n> ?- recipe_details(\'pork strips\').                                   <',[]),
   format('~n>                                                                     <',[]),
   format('~n>~c[1m ------------------------------------------------------------------- ~c[0m<',[27,27]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- initialization(info).