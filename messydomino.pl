
/*uncomment selected data you want to test (made them one-liners to be easier)*/

/*example_data*/
dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6),(4,4),(4,5),(4,6),(5,5),(5,6),(6,6)]).
frame([[3,1,2,6,6,1,2,2],[3,4,1,5,3,0,3,6],[5,6,6,1,2,4,5,0],[5,6,4,1,3,3,0,0],[6,1,0,6,3,2,4,0],[4,1,5,2,4,3,5,5],[4,1,0,2,4,5,2,0]]).


/*big_data*/
% dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),(5,5),(5,6),(5,7),(5,8),(5,9),(5,a),(6,6),(6,7),(6,8),(6,9),(6,a),(7,7),(7,8),(7,9),(7,a),(8,8),(8,9),(8,a),(9,9),(9,a),(a,a)]).
% frame([[6,5,0,5,5,3,3,1,1,4,6],[1,2,2,a,a,5,7,0,1,0,7],[5,8,6,0,8,0,9,7,7,4,2],[4,0,9,0,7,7,9,9,8,8,0],[1,a,3,8,8,5,a,8,0,0,3],[9,2,3,5,7,6,9,1,6,3,9],[2,2,2,5,8,6,0,4,6,a,a],[9,4,2,1,7,9,5,4,a,4,a],[9,a,4,9,5,5,6,6,0,a,2],[1,a,1,2,1,1,8,2,2,7,8],[7,7,3,3,4,3,6,6,4,3,1],[5,9,6,3,3,a,7,4,4,8,8]]).


/*big_big_data*/
% dominos([(a,b),(b,c),(c,d),(d,e),(e,f),(f,g),(g,h),(h,i),(i,j),(j,k),(k,l),(l,m),         (a,c),(b,d),(c,e),(d,f),(e,g),(f,h),(g,i),(h,j),(i,k),(j,l),(k,m),(l,n),         (a,d),(b,e),(c,f),(d,g),(e,h),(f,i),(g,j),(h,k),(i,l),(j,m),(k,n),(l,o),         (a,e),(b,f),(c,g),(d,h),(e,i),(f,j),(g,k),(h,l),(i,m),(j,n),(k,o),(l,p),         (a,f),(b,g),(c,h),(d,i),(e,j),(f,k),(g,l),(h,m),(i,n),(j,o),(k,p),(l,q),         (a,g),(b,h),(c,i),(d,j),(e,k),(f,l),(g,m),(h,n),(i,o),(j,p),(k,q),(l,r),         (a,h),(b,i),(c,j),(d,k),(e,l),(f,m),(g,n),(h,o),(i,p),(j,q),(k,r),(l,s),         (a,i),(b,j),(c,k),(d,l),(e,m),(f,n),(g,o),(h,p),(i,q),(j,r),(k,s),(l,t),         (a,j),(b,k),(c,l),(d,m),(e,n),(f,o),(g,p),(h,q),(i,r),(j,s),(k,t),(l,u),         (a,k),(b,l),(c,m),(d,n),(e,o),(f,p),(g,q),(h,r),(i,s),(j,t),(k,u),(l,v),         (a,l),(b,m),(c,n),(d,o),(e,p),(f,q),(g,r),(h,s),(i,t),(j,u),(k,v),(l,w),         (a,m),(b,n),(c,o),(d,p),(e,q),(f,r),(g,s),(h,t),(i,u),(j,v),(k,w),(l,x)]).
% frame([[d,g,i,r,d,f,g,l,n,f,i,s,f,k,w,l],       [k,e,a,j,k,e,s,k,j,k,b,i,r,c,j,o],       [l,q,j,p,n,h,k,l,s,j,r,t,f,v,k,k],       [x,k,a,d,f,m,m,o,c,g,d,h,j,i,c,u],       [g,q,i,b,m,a,f,e,i,b,l,a,e,i,f,g],       [n,a,o,i,p,g,r,l,r,h,a,o,g,l,p,i],       [d,c,d,g,e,f,n,h,b,t,j,e,d,h,c,i],       [i,m,g,b,q,i,b,f,c,l,l,b,u,i,h,t],       [j,h,c,g,f,a,s,l,f,l,e,c,d,j,i,j],       [p,s,n,d,a,p,c,l,b,e,k,j,u,t,h,g],       [c,f,g,g,b,h,n,e,j,h,m,i,j,f,h,c],       [f,l,w,h,e,o,h,j,k,j,v,d,b,b,n,k],       [h,r,g,n,m,d,a,d,h,l,k,b,h,m,a,i],       [o,j,l,e,k,g,d,m,e,h,k,r,j,j,l,k],       [e,c,o,h,a,n,f,k,d,q,k,k,a,j,f,p],       [v,l,i,q,p,p,k,o,m,e,d,l,g,m,k,s],       [e,n,i,g,e,q,l,m,i,o,g,m,i,c,l,k],       [q,n,j,q,l,h,f,o,b,j,p,c,l,l,u,t]]).



% (0, 0) - [[(3, 8), (4, 8)], [(4, 7), (4, 8)], [(4, 8), (5, 8)]]
% (0, 1) - [[(2, 6), (1, 6)], [(5, 3), (5, 2)], [(7, 3), (7, 2)]]
% (0, 2) - [[(7, 3), (7, 4)], [(7, 8), (7, 7)]]
% (0, 3) - [[(2, 6), (2, 5)], [(2, 6), (2, 7)], [(4, 7), (4, 6)]]
% (0, 4) - [[(2, 6), (3, 6)], [(4, 7), (5, 7)], [(5, 3), (4, 3)], [(5, 8), (5, 7)]]
% (0, 5) - [[(3, 8), (3, 7)], [(4, 7), (3, 7)], [(5, 3), (6, 3)], [(5, 8), (6, 8)], [(7, 3), (6, 3)], [(7, 8), (6, 8)]]
% (0, 6) - [[(3, 8), (2, 8)], [(5, 3), (5, 4)]]
% (1, 1) - [[(3, 4), (4, 4)], [(5, 2), (6, 2)], [(6, 2), (7, 2)]]
% (1, 2) - [[(1, 2), (1, 3)], [(1, 6), (1, 7)], [(2, 3), (1, 3)], [(3, 4), (3, 5)]]
% (1, 3) - [[(1, 2), (1, 1)], [(4, 4), (4, 5)]]
% (1, 4) - [[(1, 2), (2, 2)], [(2, 3), (2, 2)], [(4, 4), (4, 3)], [(6, 2), (6, 1)], [(7, 2), (7, 1)]]
% (1, 5) - [[(2, 3), (2, 4)], [(3, 4), (2, 4)], [(6, 2), (6, 3)]]
% (1, 6) - [[(1, 6), (1, 5)], [(2, 3), (3, 3)], [(3, 4), (3, 3)], [(4, 4), (5, 4)], [(5, 2), (4, 2)], [(5, 2), (5, 1)]]
% (2, 2) - [[(1, 7), (1, 8)], [(6, 4), (7, 4)]]
% (2, 3) - [[(1, 7), (2, 7)], [(3, 5), (2, 5)], [(3, 5), (4, 5)], [(5, 6), (4, 6)], [(5, 6), (5, 5)], [(5, 6), (6, 6)]]
% (2, 4) - [[(3, 5), (3, 6)], [(5, 6), (5, 7)], [(6, 4), (6, 5)], [(7, 4), (7, 5)]]
% (2, 5) - [[(6, 4), (6, 3)], [(7, 7), (6, 7)], [(7, 7), (7, 6)]]
% (2, 6) - [[(1, 3), (1, 4)], [(1, 8), (2, 8)], [(6, 4), (5, 4)]]
% (3, 3) - [[(1, 1), (2, 1)], [(4, 5), (4, 6)], [(4, 5), (5, 5)]]
% (3, 4) - [[(2, 1), (2, 2)], [(4, 6), (3, 6)], [(5, 5), (6, 5)], [(6, 6), (6, 5)]]
% (3, 5) - [[(2, 1), (3, 1)], [(2, 5), (2, 4)], [(2, 7), (3, 7)], [(6, 6), (6, 7)], [(6, 6), (7, 6)]]
% (3, 6) - [[(2, 5), (1, 5)], [(2, 7), (2, 8)], [(5, 5), (5, 4)]]
% (4, 4) - [[(6, 1), (7, 1)], [(6, 5), (7, 5)]]
% (4, 5) - [[(3, 6), (3, 7)], [(5, 7), (6, 7)], [(7, 5), (7, 6)]]
% (4, 6) - [[(2, 2), (3, 2)], [(4, 3), (3, 3)], [(4, 3), (4, 2)], [(6, 1), (5, 1)]]
% (5, 5) - [[(3, 1), (4, 1)], [(6, 7), (6, 8)]]
% (5, 6) - [[(2, 4), (1, 4)], [(3, 1), (3, 2)], [(4, 1), (4, 2)], [(4, 1), (5, 1)]]
% (6, 6) - [[(1, 4), (1, 5)], [(3, 2), (3, 3)], [(3, 2), (4, 2)]]


% [(4, 8), (5, 8)]
% [(2, 6), (1, 6)]
% [(7, 3), (7, 4)]
% [(4, 7), (4, 6)]
% [(5, 3), (4, 3)]
% [(7, 8), (6, 8)]
% [(3, 8), (2, 8)]
% [(3, 4), (4, 4)]
% [(2, 3), (1, 3)]
% [(1, 2), (1, 1)]
% [(7, 2), (7, 1)]
% [(6, 2), (6, 3)]
% [(5, 2), (4, 2)]
% [(1, 7), (1, 8)]
% [(5, 6), (6, 6)]
% [(3, 5), (3, 6)]
% [(7, 7), (7, 6)]
% [(6, 4), (5, 4)]
% [(4, 5), (5, 5)]
% [(2, 1), (2, 2)]
% [(2, 7), (3, 7)]
% [(2, 5), (1, 5)]
% [(6, 5), (7, 5)]
% [(5, 7), (6, 7)]
% [(6, 1), (5, 1)]
% [(3, 1), (4, 1)]
% [(2, 4), (1, 4)]
% [(3, 2), (3, 3)]




put_dominos:-
  dominos(D),
  length(D,N),
  length(Pos,N),
  make_domains(D,Pos,Domains),
  solve(Domains),
  frame(F),
  print_solution(Pos,F,1).


  % print_pos([]).
  % print_pos([D-X-_|T]):-
    % writeln(D-X),
    % print_pos(T).

print_solution(Pos,[Row],I):-!,
  print_row(Pos,Row,I,1,[],_),nl.

print_solution(Pos,[Row|Rows],I):-
  print_row(Pos,Row,I,1,[],NL),
  print_NextLine(NL),
  I1 is I+1,
  print_solution(Pos,Rows,I1).

print_NextLine([X]):-!,
  writeln(X).

print_NextLine([X|T]):-
  write(X),write(' '),
  print_NextLine(T).
  

get_paired(X,[[X,Y]|_],Y):-!.

get_paired(X,[[Y,X]|_],Y):-!.

get_paired(X,[_|Rest],Y):-
  get_paired(X,Rest,Y).


print_row(Pos,[X],I,J,SFNextLine,SFNextLine2):-!,
  writeln(X),
  get_paired((I,J),Pos,(Ip,Jp)),
  (Ip is I + 1, Jp = J -> 
      append(SFNextLine,['|'],SFNextLine2)
    ;
    append(SFNextLine,[' '],SFNextLine2)
  ).
  
print_row(Pos,[X|T],I,J,SFNextLine,NextLine):-
  write(X),
  J1 is J+1,
  get_paired((I,J),Pos,(Ip,Jp)),
  (I = Ip , Jp = J1->
    write('-'),append(SFNextLine,[' '],SFNextLine2)
  ;
    write(' '),
    (Ip is I + 1, Jp = J -> 
      append(SFNextLine,['|'],SFNextLine2)
    ;
    append(SFNextLine,[' '],SFNextLine2)
  )),
  print_row(Pos,T,I,J1,SFNextLine2,NextLine).

solve([]).
solve(DomList):-
  mrv(DomList,D,Pos,Dom,Rest),
  member(Pos,Dom),
  update_domains(D,Pos,Rest,Updated),
  solve(Updated).

mrv([D-Pos-Dom],D,Pos,Dom,[]).

mrv([D1-Pos1-Dom1|Rest1],DMin,PosMin,DomMin,Rest):-
  mrv(Rest1,D2,Pos2,Dom2,Rest2),
  length(Dom1,N1),
  length(Dom2,N2),
  (N1 < N2 ->
    (DMin = D1,
    PosMin = Pos1,
    DomMin = Dom1,
    Rest = Rest1)
  ; 
    (DMin = D2,
    PosMin = Pos2,
    DomMin = Dom2,
    Rest = [D1-Pos1-Dom1|Rest2])
  ).


% update_domains((0,0),[(3, 8), (4, 8)],
% [
% [[(2, 6), (1, 6)], [(5, 3), (5, 2)], [(7, 3), (7, 2)]],
% [[(7, 3), (7, 4)], [(7, 8), (7, 7)]],
% [[(2, 6), (2, 5)], [(2, 6), (2, 7)], [(4, 7), (4, 6)]],
% [[(2, 6), (3, 6)], [(4, 7), (5, 7)], [(5, 3), (4, 3)], [(5, 8), (5, 7)]]
% ],Updated).

count_sol(N):-
  findall(_,put_dominos,S),
  length(S,N).



update_domains(_,_,[],[]).

update_domains(D,Pos,[D1-Pos1-Dom1|Doms],[D1-Pos1-UDom1|UDoms]):-
  update_domain(Pos,Dom1,[],UDom1),
  update_domains(D,Pos,Doms,UDoms).


update_domain(_,[],SFNdom,SFNdom).

update_domain([(Ia,Ja),(Ib,Jb)],[Pos1|Rest],SFNdom,Ndom):-
  (member((Ia,Ja),Pos1) ; member((Ib,Jb),Pos1)) ->
    % append(SFNdom,[],SFNdom1),
    update_domain([(Ia,Ja),(Ib,Jb)],Rest,SFNdom,Ndom)
  ; append(SFNdom,[Pos1],SFNdom1),
    update_domain([(Ia,Ja),(Ib,Jb)],Rest,SFNdom1,Ndom).




print_domains([]).

print_domains([Dom|Doms]):-
  writeln(Dom),
  print_domains(Doms).

  

make_domains([],_,[]).

make_domains([X|Dt],[Pos|T],[X-Pos-Dom|Doms]):-
  make_domains(Dt,T,Doms),
  make_domain(X,Dom).



% make_domain((A,A),Domain):-
%   find_all(L,make_domain((A,A),L),Dom),
%   remove_symmetric(Dom,Domain).

% make_domain((A,B),[[(Ya,Xa),(Yb,Xb)]]):-
%   get_tile(Ya,Xa,A),
%   get_tile(Yb,Xb,B),
%   neighbors(Ya,Xa,Yb,Xb).  
  
% make_domain((A,B),[[(Ya,Xa),(Yb,Xb)]]):-
  % get_tile(Ya,Xa,A),
  % neighbor(Ya,Xa,B,(Yb,Xb)).


% make_domain((A,B),[[Colh|Colt]|Rt],[(Ya,Xa),(Yb,Xb)|T]):-
  % make_domain((A,B),[[Colt]|Rt],T).

col_search(_,_,_,[],SFDom,SFDom).

col_search((A,A),J,I,[A|T1],SFDom1,SFDom):- !,
  Up is I-1,
  Down is I+1,
  Right is J+1,
  Left is J-1,
  (get_tile(Up,J,A ),\+member([(Up,J),(I,J)],SFDom1)->
    append(SFDom1,[[(I,J),(Up,J)]],SFDom2) 
  ;
    append(SFDom1,[],SFDom2)
  ),
  (get_tile(Down,J,A),\+member([((Down,J),I,J)],SFDom1)->
    append(SFDom2,[[(I,J),(Down,J)]],SFDom3) 
  ;
    append(SFDom2,[],SFDom3)
  ),
  (get_tile(I,Left,A),\+member([(I,Left),(I,J)],SFDom1) ->
    append(SFDom3,[[(I,J),(I,Left)]],SFDom4)
  ; 
    append(SFDom3,[],SFDom4)
  ),
  (get_tile(I,Right,A),\+member([(I,Right),(I,J)],SFDom1) ->
    append(SFDom4,[[(I,J),(I,Right)]],SFDom5)
  ; 
    append(SFDom4,[],SFDom5)
  ),
  col_search((A,A),Right,I,T1,SFDom5,SFDom).

col_search((A,B),J,I,[A|T1],SFDom1,SFDom):- !,
  Up is I-1,
  Down is I+1,
  Right is J+1,
  Left is J-1,
  (get_tile(Up,J,B ) ->
    append(SFDom1,[[(I,J),(Up,J)]],SFDom2) 
  ;
    append(SFDom1,[],SFDom2)
  ),
  (get_tile(Down,J,B) ->
    append(SFDom2,[[(I,J),(Down,J)]],SFDom3) 
  ;
    append(SFDom2,[],SFDom3)
  ),
  (get_tile(I,Left,B) ->
    append(SFDom3,[[(I,J),(I,Left)]],SFDom4)
  ; 
    append(SFDom3,[],SFDom4)
  ),
  (get_tile(I,Right,B) ->
    append(SFDom4,[[(I,J),(I,Right)]],SFDom5)
  ; 
    append(SFDom4,[],SFDom5)
  ),
  col_search((A,B),Right,I,T1,SFDom5,SFDom).
  

col_search((A,B),J,I,[_|T],SFDom1,SFDom):-
  J1 is J+1,
  col_search((A,B),J1,I,T,SFDom1,SFDom).

row_search(_,_,[],SFDom,SFDom).

row_search((A,B),I,[Rh|Rt],SFDom,Dom):-
  col_search(A,1,I,Rh,Dom1),
  I1 is I + 1,
  append(SFDom,Dom1,SFDom1),
  row_search((A,B),I1,Rt,SFDom1,Dom).


find_in_row(_,_,_,[],[]).

find_in_row(X,I,J,[X|T],[(I,J)|T1]):- !,
  J1 is J+1,
  find_in_row(X,I,J1,T,T1).

find_in_row(X,I,J,[_|T],Occ):-
  J1 is J+1,
  find_in_row(X,I,J1,T,Occ).

find_in_frame(_,_,[],SFOcc,SFOcc).

find_in_frame(X,I,[Row|Rows],SFOcc1,Occ):-
  find_in_row(X,I,1,Row,SFOcc2),
  append(SFOcc1,SFOcc2,SFOcc3),
  I1 is I+1,
  find_in_frame(X,I1,Rows,SFOcc3,Occ).

find_all_occurrences(X,Occ):-
  frame(F),
  find_in_frame(X,1,F,[],Occ).




asymmetric_neighbors([],[],SFDom,SFDom).

asymmetric_neighbors([_|Ta],[],SFDom1,Dom):-
  asymmetric_neighbors(Ta,Ta,SFDom1,Dom).


asymmetric_neighbors([(Ia,Ja)|Ta],[(Ib,Jb)|Tb],SFDom1,Dom):-
  (((Ia is Ib + 1; Ia is Ib - 1),Ja = Jb ; (Ja is Jb + 1; Ja is Jb - 1), Ia = Ib) ->
    append(SFDom1,[[(Ia,Ja),(Ib,Jb)]],SFDom2)
  ; 
    append(SFDom1,[],SFDom2)
  ),
  asymmetric_neighbors([(Ia,Ja)|Ta],Tb,SFDom2,Dom).


make_domain((A,A),Dom):-!,
  find_all_occurrences(A,[H|T]),
  asymmetric_neighbors([H|T],T,[],Dom).



make_domain((A,B),Dom):-
  find_all_occurrences(A,Aocc),
  find_all_occurrences(B,Bocc),
  neighbors(Aocc,Bocc,Bocc,[],Dom).




neighbors([],_,_,SFDom,SFDom).

neighbors([_|Ta],Bocc,[],SFDom1,Dom):-
  neighbors(Ta,Bocc,Bocc,SFDom1,Dom).
  

neighbors([(Ia,Ja)|Ta],Bocc,[(Ib,Jb)|Tb],SFDom1,Dom):-
  (((Ia is Ib + 1; Ia is Ib - 1),Ja = Jb ; (Ja is Jb + 1; Ja is Jb - 1), Ia = Ib) ->
    append(SFDom1,[[(Ia,Ja),(Ib,Jb)]],SFDom2)
  ; 
    append(SFDom1,[],SFDom2)
  ),
   neighbors([(Ia,Ja)|Ta],Bocc,Tb,SFDom2,Dom).




% neighbor(Ya,Xa,B,(Yb,Xb)):-
  % (Yb is Ya + 1, get_tile(Yb,Xa,B) ;
  % Yb is Ya - 1, get_tile(Yb,Xa,B)), Xb is Xa ;
  % (Xb is Xa + 1, get_tile(Ya,Xb,B) ;
  % Xb is Xa - 1, get_tile(Ya,Xb,B)), Yb is Ya.


% neighbors(Ya,Xa,Yb,Xb):-
  % (Xa = Xb -> (Ya - Yb =:= 1 ; Yb - Ya =:= 1);(Ya = Yb -> (Xa - Xb =:= 1 ; Xb - Xa =:= 1))).

get_tile(Y,X,A):-
  frame([Rh|Rt]),
  (var(Y) -> 
    width([Rh|Rt],W),
    between(1,W,Y)
  ; true),
  get_row(Y,[Rh|Rt],Row),
  (var(X) ->
    length(Rh,L),
    between(1,L,X)
  ; true),
  get_col(X,Row,A).


between(LBound, RBound, LBound) :-
  LBound =< RBound. 
between(LBound, RBound, Result) :-
  LBound < RBound,
  NextLBound is LBound + 1,
  between(NextLBound, RBound, Result).

get_row(1,[Row|_],Row).

get_row(Y,[_|Rt],Row):-
  Y>1,
  Y1 is Y-1,
  get_row(Y1,Rt,Row).


get_col(1,[X|_],X).

get_col(I,[_|T],X):-
  I > 1,
  I1 is I-1,
  get_col(I1,T,X).

