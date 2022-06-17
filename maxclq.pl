:- lib(ic).
:- lib(branch_and_bound).
:-compile(create_graph).

maxclq(N,D,Clique,Size):-
  create_graph(N,D,G),
  def_vars(N,Nodes),
  state_const(Nodes,1,2,G),
  Size1 #= -sum(Nodes),
  bb_min(search(Nodes,0,input_order,indomain,complete,[]),Size1,_),
  Size is -1*Size1,
  find_ones(Nodes,1,Clique).

find_ones([],_,[]).

find_ones([Node|Rest1],I,Clique):-
  Node \= 1,
  I1 is I + 1,
  find_ones(Rest1,I1,Clique).



find_ones([Node|Rest1],I,[I|Rest2]):-
  Node =:= 1,
  I1 is I+1,
  find_ones(Rest1,I1,Rest2).


def_vars(N,Nodes):-
  length(Nodes, N),
  Nodes #:: 0..1.
  

state_const(Nodes,N,_,_):-
  length(Nodes, N).
  

state_const(Nodes,V,N1,G):-
  length(Nodes, N),
  N1 is N + 1,
  V1 is V+1,
  V2 is V1 + 1,
  state_const(Nodes,V1,V2,G).
  

  

state_const(Nodes,V1,V2,G):-
  ((\+member(V1-V2,G))->
  n_th(V1,Nodes,Node1),
  n_th(V2,Nodes,Node2),
  Node1+Node2 #< 2;true),
  V is V2 + 1,
  state_const(Nodes,V1,V,G).

n_th(1, [Node|_], Node).

n_th(N, [_| Nodes], Node) :-
  N \= 1,
  N1 is N - 1,
  n_th(N1, Nodes, Node).