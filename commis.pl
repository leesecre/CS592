:- module(commis,[commis/2]).

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



