/* Other modules */
:- use_module(readLine,[readLine/1]).
:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                printRepresentations/1,
				compose/3]).

:- use_module(betaConversion,[betaConvert/2]).

:- use_module(sentenceTestSuite,[sentence/2]).


:- [englishGrammar].

:- [englishLexicon].

:- [semLexLambda].

:- [semRulesLambda].

:- [query].

commis(Sentence,Sems):-
	setof(Sem,t([sem:Sem],Sentence,[]),Sems).


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
    read_line_to_string(user_input, SubstituteString),
    string_lower(SubstituteString, Substitute), % Convert to lowercase for consistency
    (Substitute \= "none" 
    -> (atom_string(SubstituteAtom, Substitute),
        assertz(ingredient_substitute(SubstituteAtom, Ingredient)),
        ask_for_substitute(Ingredient))
    ;true).

/* [excluded_ingredients] */
ask_for_excluded_ingredients :-
    format('Is there any you cannot eat? (type "none" when finished): '),
    read_line_to_string(user_input, IngredientString),
    string_lower(IngredientString, Ingredient), % Convert to lowercase for consistency
    (Ingredient \= "none" 
    -> (atom_string(IngredientAtom, Ingredient), % Convert string to atom
        assertz(excluded_ingredient(IngredientAtom)),
        ask_for_excluded_ingredients)
    ; true).

list_excluded_ingredients(List) :-
    findall(Ingredient, excluded_ingredient(Ingredient), List).

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
    findall((Recipe, Length), (
        recipe(Recipe, RecipeIngredients, _, _),
        ingredients_available_or_substituted(RecipeIngredients, AvailableIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 4  % To avoid abnormal data
    ), CompleteMatches),
    findall((Recipe, MissingIngredients), (
        recipe(Recipe, RecipeIngredients, _, _),
        member_of_both(AvailableIngredients, RecipeIngredients),
        \+ ingredients_available_or_substituted(RecipeIngredients, AvailableIngredients),
        \+ subset(RecipeIngredients, AvailableIngredients),
        \+ equivalent_lists(AvailableIngredients, RecipeIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 4,
        find_missing_ingredients(AvailableIngredients, RecipeIngredients, MissingIngredients),
        length(MissingIngredients, MissLength),
        MissLength < 2
    ), PartialMatches),
    print_complete_matches(CompleteMatches),
    format('~n~c[31m[Recipes with some ingredients missing]~c[0m                 \n',[27,27]),
    print_partial_matches(PartialMatches, 20).

% Predicate to recommend recipes based on available ingredients
recommend_recipes(AvailableIngredients) :-
    findall((Recipe, Length), (
        recipe(Recipe, RecipeIngredients, _, _),
        ingredients_available_or_substituted(RecipeIngredients, AvailableIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 4  % To avoid abnormal data
    ), CompleteMatches),
    findall((Recipe, MissingIngredients), (
        recipe(Recipe, RecipeIngredients, _, _),
        member_of_both(AvailableIngredients, RecipeIngredients),
        \+ ingredients_available_or_substituted(RecipeIngredients, AvailableIngredients),
        \+ subset(RecipeIngredients, AvailableIngredients),
        \+ equivalent_lists(AvailableIngredients, RecipeIngredients),
        not_contains_excluded_ingredients(RecipeIngredients),
        length(RecipeIngredients, Length),
        Length > 4,
        find_missing_ingredients(AvailableIngredients, RecipeIngredients, MissingIngredients),
        length(MissingIngredients, MissLength),
        MissLength < 2
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
    ;   print_list_row(List) 
    ),
    write(']\n').

print_first_cut(_, 0) :- 
    write('...').

print_first_cut([H|T], N) :-
    N > 0,
    write(H),
    (   T == []  % Check if the tail is empty
    ->  true     % Do nothing for the last element
    ;   write(', '),  % Otherwise, print a comma
        N1 is N - 1,
        print_first_cut(T, N1)
    ).

print_list_row([]).
print_list_row([H|T]) :-
    write(H),
    (   T == []  % Check if the tail is empty
    ->  true     % Do nothing for the last element
    ;   write(', '),  % Otherwise, print a comma
        print_list_row(T)
    ).

print_list_bold([]).
print_list_bold([Head|Tail]) :-
    format('* ~c[1m~w~c[0m\n', [27,Head,27]),
    print_list_bold(Tail).

print_list_tuple_bold([]).
print_list_tuple_bold([(H1, H2)|Tail]) :-
    format('* ~c[1m~w~c[0m number of ingredients: ~w.\n', [27,H1,27, H2]),
    print_list_tuple_bold(Tail).

% Print complete matches
print_complete_matches([]) :-
    format('~n[No recipes found with all ingredients available]           \n',[]).
print_complete_matches(CompleteMatches) :-
    format('~n~c[34m[Recipes with all ingredients available]~c[0m                \n',[27,27]),
    print_list_tuple_bold(CompleteMatches).

print_partial_matches([], _) :-
    format('~n[No recipes found with partial ingredients ~c[34mavailable~c[0m]          \n',[27,27]).
print_partial_matches(_, 0). % Stop printing after 10 matches
print_partial_matches([(Recipe, MissingIngredients) | Rest], Counter) :-
    Counter > 0,
    format('* ~c[1m~w~c[0m\n  -Missing ingredients: ', [27,Recipe,27]),
    print_list_skip(MissingIngredients),
    NewCounter is Counter - 1,
    print_partial_matches(Rest, NewCounter).



% Predicate to print ingredients, highlighting missing ones in red
print_ingredients([]).  % Base case: do nothing for an empty list.
print_ingredients([Ingredient]) :-
    !,  % Cut to prevent backtracking
    print_ingredient(Ingredient).
print_ingredients([Ingredient|Rest]) :-
    print_ingredient(Ingredient),
    write(', '),
    print_ingredients(Rest).

print_ingredient(Ingredient) :-
    available_ingredient(Ingredient),
    format('~w', [Ingredient]).
print_ingredient(Ingredient) :-
    \+ available_ingredient(Ingredient),
    print_red(Ingredient).

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

% Checks if all ingredients are available or have a substitute
ingredients_available_or_substituted(RecipeIngredients, AvailableIngredients) :-
    forall(member(Ingredient, RecipeIngredients),
           (member(Ingredient, AvailableIngredients) ; substitute_available(Ingredient, AvailableIngredients))).

% Checks if there is an available substitute for the given ingredient
substitute_available(Ingredient, AvailableIngredients) :-
    ingredient_substitute(Ingredient, Substitute),
    member(Substitute, AvailableIngredients).


/* USER INTERFACE */
assert_avail_ingredients([]). % Base case: do nothing for an empty list.
assert_avail_ingredients([H|T]) :-
    assertz(available_ingredient(H)), % Assert the head of the list.
    assert_avail_ingredients(T). % Recursively assert the rest of the list.

commis_add:-
    format('~c[1mcommis:~c[0m Okay, What do you have? \n',[27,27]),
    readLine(Sentence),
    commis(Sentence,Sems),
    nth0(0,Sems,Sem),
    extract_constants(Sem,Ingredients),
    assert_avail_ingredients(Ingredients),
    list_available_ingredients(AvailableIngredients),
    format("~c[1mcommis:~c[0m Then now available ingredients are \n",[27,27]),
    format('* ['),
    print_list_row(AvailableIngredients),
    format(']\n').


delete_avail_ingredient(Ingredient) :-
    retractall(available_ingredient(Ingredient)).

delete_avail_ingredients([]).
delete_avail_ingredients([H|T]) :-
    retractall(available_ingredient(H)),
    delete_avail_ingredients(T).

commis_del:-
    format('~c[1mcommis:~c[0m Which ingredient would you like to delete? \n',[27,27]),
    readLine(Ingredient),
    delete_avail_ingredient(Ingredient),
    list_available_ingredients(AvailableIngredients),
    format("~c[1mcommis:~c[0m Updated available ingredients are \n",[27,27]),
    format('* ['),
    print_list_row(AvailableIngredients),
    format(']\n').

assert_exc_ingredients([]). % Base case: do nothing for an empty list.
assert_exc_ingredients([H|T]) :-
    assertz(excluded_ingredient(H)), % Assert the head of the list.
    assert_exc_ingredients(T). % Recursively assert the rest of the list.

commis_exclude:-
    format('~c[1mcommis:~c[0m Is there any you cannot eat? \n',[27,27]),
    readLine(Sentence),
    commis(Sentence,Sems),
    nth0(0,Sems,Sem),
    extract_constants(Sem,Ingredients),
    assert_exc_ingredients(Ingredients),
    list_excluded_ingredients(AvailableIngredients),
    format("~c[1mcommis:~c[0m I recorded what you cannot eat here. \n",[27,27]),
    format('* ['),
    print_list_row(AvailableIngredients),
    format(']\n').


commis_sub:-
    format('TODO').

commis_ask:-
    recommend_recipes.

commis_detail:-
    format('~c[1mcommis:~c[0m which recipe you want to know? \n',[27,27]),
    readLine(Sentence),
    commis(Sentence,Sems),
    nth0(0,Sems,Sem),
    extract_constants(Sem,Recipe),
    recipe_details(Recipe).

hey_commis :-
    format('~c[1mcommis:~c[0m Hello, do you need help? :)\n',[27,27]),
    format('    1.  (type "add") tell commis adding available ingredients.\n'),
    format('    2.  (type "del") tell commis deleting available ingredients.\n'),
    format('    3.  (type "exc") tell commis ingredients you can\'t eat.\n'),
    format('    4.  (type "sub") tell commis substitute for some ingredients.\n'),
    format('    5.  (type "ask") ask commis possible recipe for now.\n'),
    format('    6.  (type "det") ask commis for the details of a recipe.\n'),
    read_line_to_string(user_input, Command),
    (Command = "add" -> commis_add, !;
     Command = "del" -> commis_del, !;
     Command = "exc" -> commis_exclude, !;
     Command = "sub" -> commis_sub, !;
     Command = "ask" -> commis_ask, !;
     Command = "det" -> commis_detail, !;
     format("~c[1mcommis:~c[0m ~c[31m(ERROR) I cannot understand what yor say.~c[0m\n",[27,27,27,27]), false).

/*========================================================================
   Start Interface.
========================================================================*/

info:- 
   format('~n>~c[1m ----------------------------------------------------------------------------- ~c[0m<',[27, 27]),
    format('~n'),
    format('       .--,--.~n'),
    format('       `.  ,.''~n'),
    format('        |___|~n'),
    format('        :o o:    O    ~c[1mCOMMIS~c[0m, your food recommdender!~n',[27, 27]),
    format('       _`.v.''_   |    by Jaehwan Lee and Insun Baek ~n'),
    format('     /''   ^    `W=)      version 1.0 ~n'),
    format('   .''  _______ ''-|~n'),
    format('   `(<=|     |= /''~n'),
    format('       |     |~n'),
    format('       |_____|~n'),
    format('--------=====-------------'),
   format('~n>                                                                               <',[]),
   format('~n> ?- ~c[32mload_all.~c[0m      - Load recipes and user data from storage                   <',[27,27]),
   format('~n> ?- ~c[32msave_all.~c[0m      - Save recipes and user data to storage                     <',[27,27]),
   format('~n> ?- ~c[32mhey_commis.~c[0m    - Ask commis, interface to communicate                      <',[27,27]),
   format('~n> ?-      ~c[32m(add)~c[0m     - add available ingredients                                 <',[27,27]),
   format('~n> ?-      ~c[32m(del)~c[0m     - delete available ingredients                              <',[27,27]),
   format('~n> ?-      ~c[32m(exc)~c[0m     - exclude some ingredients                                  <',[27,27]),
   format('~n> ?-      ~c[32m(sub)~c[0m     - tell substitute of ingredients                            <',[27,27]),
   format('~n> ?-      ~c[32m(ask)~c[0m     - ask possible recipe                                       <',[27,27]),
   format('~n> ?-      ~c[32m(det)~c[0m     - ask recipe details                                        <',[27,27]),
   format('~n>                                                                               <',[]),
   format('~n>~c[1m ----------------------------------------------------------------------------- ~c[0m<',[27,27]),
   format('~n~n',[]).

:- initialization(info).