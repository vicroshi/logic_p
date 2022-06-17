:-lib(ic).
:- lib(ic_global).

carseq(Vars):-
  def_vars(Vars),
  set_constraints(Vars),
  search(Vars, 0, input_order, indomain, complete, []).

def_vars(V):-
  classes(C),
  L1 is sum(C),
  length(C,L2),
  length(V,L1),
  V #:: 1..L2.

set_constraints(V):-
  options(O),
  classes(C),
  constraints(V,O,C).

constraints(V,Opt,C):-
  constr2(V,C,1),

  constr1(V,Opt).

constr2(_,[],_).

constr2(V,[C1|T],I):-
  occurrences(I,V,C1),
  I1 is I+1,
  constr2(V,T,I1).

constr1(_,[]).

constr1(V,[K/High/Conf|T1]):-
  find_ones(Conf,1,Ones),
  classes(C),
  find_max(Ones,C,L),
  Max is sum(L),
  sequence_total(Max,Max,0,High,K,V,Ones),
  constr1(V,T1).

ith(1,[X|_],X).

ith(I,[_|T],X):-
  I > 1,
  I1 is I-1,
  ith(I1,T,X).

find_max([],_,[]).

find_max([I|T1],C,[X|T2]):-
  ith(I,C,X),
  find_max(T1,C,T2).

find_ones([],_,[]).

find_ones([Node|Rest1],I,Clique):-
  Node \= 1,
  I1 is I + 1,
  find_ones(Rest1,I1,Clique).

find_ones([Node|Rest1],I,[I|Rest2]):-
  Node =:= 1,
  I1 is I+1,
  find_ones(Rest1,I1,Rest2).