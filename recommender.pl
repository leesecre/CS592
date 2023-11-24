:- dynamic recipe/2.

% Load the CSV file into facts
load_recipes :-
    write('Loading recipes...'), nl,
    retractall(recipe(_, _)), % Clear existing recipes
    csv_read_file('recipes.csv', Rows, []),
    maplist(assert_recipe, Rows),
    write('Recipes loaded successfully.').

atom_to_list(Atom, List) :-
    read_term_from_atom(Atom, List, []).

assert_recipe(Row) :-
    Row = row(Recipe, IngredientsAtom),
    atom_to_list(IngredientsAtom, Ingredients),
    assertz(recipe(Recipe, Ingredients)).

% Predicate to recommend recipes based on available ingredients
recommend_recipes(AvailableIngredients) :-
    findall(Recipe, (
        recipe(Recipe, RecipeIngredients),
        subset(AvailableIngredients, RecipeIngredients)
    ), RecommendedRecipes),
    print_recommendations(RecommendedRecipes).

print_recommendations([]) :-
    write('No matching recipes found with the available ingredients.').
print_recommendations(Recipes) :-
    write('Recommended recipes: '), nl,
    print_recipes(Recipes).

print_recipes([]).
print_recipes([Recipe|Rest]) :-
    write('- '), write(Recipe), nl,
    print_recipes(Rest).


% Example usage:
% To load the recipes from the CSV file:
% ?- load_recipes.

% To recommend recipes with available ingredients:
% ?- recommend_recipes(['sugar', 'salt']).