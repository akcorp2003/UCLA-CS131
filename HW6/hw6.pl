/*	Name: Aland Kuang
	UID: 104157973

*/

duplist([],[]).
duplist([X|T1], [X,X|T2]) :- duplist(T1, T2).

subseq([],_).
subseq([H1|T1],[H2|T2]) :- H1=H2, subseq(T1,T2).
subseq([H1|T1],[_|T2]) :- subseq([H1|T1], T2).


/* Solution is adapted from StackOverflow user Ygandelsman, found here:
stackoverflow.com/questions/11882760/cryptarithmetic-puzzle-prolog
and also from Prolog Programming for Aritifical Intelligence by Ivan Bratko, found on page 161 of the 1986 edition
*/

/*zeropad is using StackOverflow user hardmath's implementation, which can be found here:
http://stackoverflow.com/questions/10925285/faster-implementation-of-verbal-arithmetic-in-prolog*/

/* The problem with this implementation is that it cannot successfully find the solution for adders of different
lengths. If we set restrictions on certain variables, the algorithm will take a very long to compute.*/

verbalarithmetic(Letters, Restterm1, Restterm2, Solution) :-
	addzerotofront(Term1, Solution, Term1a),
	addzerotofront(Term2, Solution, Term2a),
	sumhelper(Term1a, Term2a, Solution, 0, 0, [0,1,2,3,4,5,6,7,8,9], _).
	/*sum1(Term1, Term2, Solution, 0, 0, [0,1,2,3,4,5,6,7,8,9], _).*/


sumhelper([], [], [], 0, 0, Numbers, Numbers).
sumhelper([D1|NumRest1], [D2|NumRest2], [D|NumRest], CarryfromRight, CarrytoLeft, Digits1, Digs) :-
	sumhelper(NumRest1, NumRest2, NumRest, CarryfromRight, SomeCarryDigit, Digits1, Digits2),
	digitsum(D1, D2, SomeCarryDigit, D, CarrytoLeft, Digits2, Digs).
	

digitsum(LV1, D1, D2, CarryfromRight, D, CarrytoLeft, Digits1, Digs) :-
	remove(D1, Digits1, Digits2),
	remove(D2, Digits2, Digits3),
	remove(D, Digits3, Digs),
	S is D1 + D2 + CarryfromRight,
	D is S mod 10,
	CarrytoLeft is S div 10.

remove(X, L, L) :-
	nonvar(X). /*X is already instantiated if it's not a variable*/
remove(X, [X|Xs], Xs).
remove(X, [Y|Ys], [Y|Rest]) :-
	remove(X,Ys,Rest). /*don't keep the item that is not removed*/

/*ins(Val,[H|List],Pos,[H|Res]):- Pos > 1, !, 
                                Pos1 is Pos - 1, ins(Val,List,Pos1,Res). 
ins(Val, List, 1, [Val|List]).*/

addzerotofront(List1, GoalList, FinalList) :-
	length(List1, X), length(GoalList, Y), (X < Y -> AddZs is Y - X, (zeropad(List1, AddZs, FinalList)) ; List1=FinalList).

zeropad(L,0,L).
zeropad(L,ToAdd, Result) :-
	ToAdd > 0,
	LefttoAdd is ToAdd - 1,
	zeropad([0|L], LefttoAdd, Result).

/* blocksworld */

/*case for Stack1 */
moveblock(world([Top|Stack1], Stack2, Stack3, none),pickup(Top, stack1), world(Stack1, Stack2, Stack3, Top)).
moveblock(world(Stack1, Stack2, Stack3, Top), putdown(Top, stack1), world([Top|Stack1], Stack2, Stack3, none)).

/*case for Stack2 */
moveblock(world(Stack1, [Top|Stack2], Stack3, none), pickup(Top, stack2), world(Stack1, Stack2, Stack3, Top)).
moveblock(world(Stack1, Stack2, Stack3, Top), putdown(Top, stack2), world(Stack1, [Top|Stack2], Stack3, none)).

/*case for Stack3 */
moveblock(world(Stack1, Stack2, [Top|Stack3], none), pickup(Top, stack3), world(Stack1, Stack2, Stack3, Top)).
moveblock(world(Stack1, Stack2, Stack3, Top), putdown(Top, stack3), world(Stack1, Stack2, [Top|Stack3], none)).

blocksworld(Goal, [], Goal).
blocksworld(Beginning, [Action|Actions], Goal) :- moveblock(Beginning, Action, NewWorld), blocksworld(NewWorld, Actions, Goal).


