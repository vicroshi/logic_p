% :-compile(idfs).

% initial_state(X).
% final_state(Y).

% codegen(L1,L2,Plan):-

% codegen(L1,L2,[]):- % incase of empty list, don't even try idfs, just output empty move list.
%   L1 = [] ; L2 = [];
%   length(L1, N1) , length(L2, N2), N1 \= N2,
%   !. 

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

% asterisk(L2,L2):-
%   not member(*,L2).


% asterisk(L2,L4):-
%   member(*,L2),
%   find_all_i(L2,1,*,IL),
%   replace_ith(I,L2,_,L3),
%   asterisk(L3,L4).

check_members(_,[]).

check_members(L,[X|T1]):-
  var(X),!,
  check_members(L,T1).
  

check_members(L,[X|T1]):-
  % nonvar(X),
  member(X,L),
  check_members(L,T1).

asterisk([],_,[]).

asterisk([X|T1],*,[X|T2]):-
  X \= *,
  asterisk(T1,*,T2).

asterisk([*|T1],*,[_|T2]):-
  asterisk(T1,*,T2).
  

find_all_i([],_,_,[]).

find_all_i([X|T1],I,X,[I|T2]):-
  I1 is I + 1,
  find_all_i(T1,I1,X,T2).

find_all_i([X|T1],I,Y,IL):-
  X \= Y,
  I1 is I + 1,
  find_all_i(T1,I1,Y,IL).
  

idfs(FL,FL,_,Plan,Plan):-!.

idfs(L1,L2,Depth,MVL,Plan):-
  dfs(L1,L2,Depth,MVL,Plan),!;
  D1 is Depth + 1,
  idfs(L1,L2,D1,MVL,Plan).

dfs(FL,FL,_,Plan,Plan).

dfs(IL,FL,Depth,MVL,Plan):-
  Depth > 0,
  legal_move(IL,Move,NL),
  D1 is Depth - 1,
  append(MVL,[Move],SFPlan),
  dfs(NL,FL,D1,SFPlan,Plan).

between(LBound, RBound, LBound) :-
  LBound =< RBound. 
between(LBound, RBound, Result) :-
  LBound < RBound,
  NextLBound is LBound + 1,
  between(NextLBound, RBound, Result).

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

swap(L1,I,J,L3):-
  length(L1, N),
  % N1 is N - 1
  between(1,N,I),
  between(1,N,J),
  I < J,
  ith(I,L1,X),
  ith(J,L1,Y),
  replace_ith(I,L1,Y,L2),
  replace_ith(J,L2,X,L3).
  

legal_move(L1,move(I),L2):-
  move(L1,I,L2).


legal_move(L1,swap(I,J),L2):-
  swap(L1,I,J,L2).



% move(IL,FL,L,IML,ML,Depth):-
%   Depth > 0,
%   D1 is Depth - 1,
%   \+equals(L,FL),
%   legal_move(L,Move,SFL),
%   append(ML, [Move], SFML),
%   move(IL,FL,SFL,IML,SFML,D1).



ith(1,[X|_],X).


ith(I,[_|T],X):-
  I > 1,
  I1 is I-1,
  ith(I1,T,X).

% move(I)

% insert_ith(1,[])
% move(I,[X|T],L2):-
%   % length(L1, N),
%   ith(I,[X|T],X),
%   I1 is I + 1,
%   replace_ith(I1,[X|T],X,L2).

i_of(X,L,I):-
  i_of(X,L,1,I).

i_of(X,[X|_],I,I).


i_of(X,[_|L],I1,I):-
  I2 is I1 + 1,
  i_of(X,L,I2,I).

replace_ith(1, [_|L], A, [A|L]).
replace_ith(I, [Y|L], A, [Y|L1]) :-
  I > 1,
  I1 is I-1,
  replace_ith(I1, L, A, L1).

/*
[a,d,a,b]
[move(2), move(1), swap(2, 3), swap(2, 4)]
*/