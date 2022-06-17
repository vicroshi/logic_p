
/**********main************/
codegen(L1,L2,Plan):-
  L1 \= [],
  L2 \= [],
  % ensure no empty list is given 
  length(L1, N),
  length(L2, N),
  % ensure initial and final lists are the same length
  asterisk(L2,*,L3),
  % replace asterisks with '_'
  check_members(L1,L3),
  % check that there are no members in final list that are not in the initial list.
  idfs(L1,L3,1,[],Plan).
/*************************/

/******check_members******/
check_members(_,[]).

check_members(L,[X|T1]):-
  var(X),!,
  check_members(L,T1).
  

check_members(L,[X|T1]):-
  member(X,L),
  check_members(L,T1).
/*************************/

/******asterisk***********/
asterisk([],_,[]).

asterisk([X|T1],*,[X|T2]):-
  X \= *,
  asterisk(T1,*,T2).

asterisk([*|T1],*,[_|T2]):-
  asterisk(T1,*,T2).
/*************************/

/*********idfs************/
idfs(FL,FL,_,Plan,Plan):-!.

idfs(L1,L2,Depth,MVL,Plan):-
  dfs(L1,L2,Depth,MVL,Plan),!;
  D1 is Depth + 1,
  idfs(L1,L2,D1,MVL,Plan).
/*************************/


/**********dfs************/
dfs(FL,FL,_,Plan,Plan).

dfs(IL,FL,Depth,MVL,Plan):-
  Depth > 0,
  legal_move(IL,Move,NL),
  D1 is Depth - 1,
  append(MVL,[Move],SFPlan),
  dfs(NL,FL,D1,SFPlan,Plan).
/*************************/

/*******legal_move********/
legal_move(L1,move(I),L2):-
  move(L1,I,L2).


legal_move(L1,swap(I,J),L2):-
  swap(L1,I,J,L2).
/*************************/

/**********move***********/
move(L,I,L2):-
  length(L, N),
  % I < N,
  % I >= 1,
  N1 is N-1,
  between(1,N1,I),
  I1 is I + 1,
  ith(I,L,X),
  replace_ith(I1,L,X,L2).

move(L,I,L2):-
  length(L,I),
  ith(I,L,X),
  replace_ith(1,L,X,L2).
/*************************/

/**********swap***********/
swap(L1,I,J,L3):-
  length(L1, N),
  between(1,N,I),
  between(1,N,J),
  I < J,
  ith(I,L1,X),
  ith(J,L1,Y),
  replace_ith(I,L1,Y,L2),
  replace_ith(J,L2,X,L3).
/*************************/

/***********ith***********/
ith(1,[X|_],X).

ith(I,[_|T],X):-
  I > 1,
  I1 is I-1,
  ith(I1,T,X).
/*************************/


/******replace_ith********/
replace_ith(1, [_|L], A, [A|L]).
replace_ith(I, [Y|L], A, [Y|L1]) :-
  I > 1,
  I1 is I-1,
  replace_ith(I1, L, A, L1).
/*************************/

/********between**********/
between(LBound, RBound, LBound) :-
  LBound =< RBound. 
between(LBound, RBound, Result) :-
  LBound < RBound,
  NextLBound is LBound + 1,
  between(NextLBound, RBound, Result).
/*************************/

