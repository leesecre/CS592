:- module(commis,[commis/0,
                  commis/2]).

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

commis:-
   write('Do you want a recipe?'),
   readLine(Sentence1),
   commis(Sentence1,Sems1),
   nth0(0, Sems1, Sem1),
   extract_constants(Sem1, Ingredients),
   write('The ingredients are: '),
   writeln(Ingredients),
   write('Is there anything you cannot eat?'),
   readLine(Sentence2),
   commis(Sentence2,Sems2),
   nth0(0, Sems2, Sem2),
   %extract_constants(Sem2, xIngredients).
   %write('The ingredients cannot eat are: '),
   writeln(xIngredients).



  
commis(Sentence,Sems):-
	setof(Sem,t([sem:Sem],Sentence,[]),Sems).



