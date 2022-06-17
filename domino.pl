
/*uncomment selected data you want to test (made them one-liners, so it's easier)*/

/*example_data*/
% dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6),(4,4),(4,5),(4,6),(5,5),(5,6),(6,6)]).
% frame([[3,1,2,6,6,1,2,2],[3,4,1,5,3,0,3,6],[5,6,6,1,2,4,5,0],[5,6,4,1,3,3,0,0],[6,1,0,6,3,2,4,0],[4,1,5,2,4,3,5,5],[4,1,0,2,4,5,2,0]]).


/*big_data*/
% dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),(5,5),(5,6),(5,7),(5,8),(5,9),(5,a),(6,6),(6,7),(6,8),(6,9),(6,a),(7,7),(7,8),(7,9),(7,a),(8,8),(8,9),(8,a),(9,9),(9,a),(a,a)]).
% frame([[6,5,0,5,5,3,3,1,1,4,6],[1,2,2,a,a,5,7,0,1,0,7],[5,8,6,0,8,0,9,7,7,4,2],[4,0,9,0,7,7,9,9,8,8,0],[1,a,3,8,8,5,a,8,0,0,3],[9,2,3,5,7,6,9,1,6,3,9],[2,2,2,5,8,6,0,4,6,a,a],[9,4,2,1,7,9,5,4,a,4,a],[9,a,4,9,5,5,6,6,0,a,2],[1,a,1,2,1,1,8,2,2,7,8],[7,7,3,3,4,3,6,6,4,3,1],[5,9,6,3,3,a,7,4,4,8,8]]).


/*big_big_data*/
% dominos([(a,b),(b,c),(c,d),(d,e),(e,f),(f,g),(g,h),(h,i),(i,j),(j,k),(k,l),(l,m),         (a,c),(b,d),(c,e),(d,f),(e,g),(f,h),(g,i),(h,j),(i,k),(j,l),(k,m),(l,n),         (a,d),(b,e),(c,f),(d,g),(e,h),(f,i),(g,j),(h,k),(i,l),(j,m),(k,n),(l,o),         (a,e),(b,f),(c,g),(d,h),(e,i),(f,j),(g,k),(h,l),(i,m),(j,n),(k,o),(l,p),         (a,f),(b,g),(c,h),(d,i),(e,j),(f,k),(g,l),(h,m),(i,n),(j,o),(k,p),(l,q),         (a,g),(b,h),(c,i),(d,j),(e,k),(f,l),(g,m),(h,n),(i,o),(j,p),(k,q),(l,r),         (a,h),(b,i),(c,j),(d,k),(e,l),(f,m),(g,n),(h,o),(i,p),(j,q),(k,r),(l,s),         (a,i),(b,j),(c,k),(d,l),(e,m),(f,n),(g,o),(h,p),(i,q),(j,r),(k,s),(l,t),         (a,j),(b,k),(c,l),(d,m),(e,n),(f,o),(g,p),(h,q),(i,r),(j,s),(k,t),(l,u),         (a,k),(b,l),(c,m),(d,n),(e,o),(f,p),(g,q),(h,r),(i,s),(j,t),(k,u),(l,v),         (a,l),(b,m),(c,n),(d,o),(e,p),(f,q),(g,r),(h,s),(i,t),(j,u),(k,v),(l,w),         (a,m),(b,n),(c,o),(d,p),(e,q),(f,r),(g,s),(h,t),(i,u),(j,v),(k,w),(l,x)]).
% frame([[d,g,i,r,d,f,g,l,n,f,i,s,f,k,w,l],       [k,e,a,j,k,e,s,k,j,k,b,i,r,c,j,o],       [l,q,j,p,n,h,k,l,s,j,r,t,f,v,k,k],       [x,k,a,d,f,m,m,o,c,g,d,h,j,i,c,u],       [g,q,i,b,m,a,f,e,i,b,l,a,e,i,f,g],       [n,a,o,i,p,g,r,l,r,h,a,o,g,l,p,i],       [d,c,d,g,e,f,n,h,b,t,j,e,d,h,c,i],       [i,m,g,b,q,i,b,f,c,l,l,b,u,i,h,t],       [j,h,c,g,f,a,s,l,f,l,e,c,d,j,i,j],       [p,s,n,d,a,p,c,l,b,e,k,j,u,t,h,g],       [c,f,g,g,b,h,n,e,j,h,m,i,j,f,h,c],       [f,l,w,h,e,o,h,j,k,j,v,d,b,b,n,k],       [h,r,g,n,m,d,a,d,h,l,k,b,h,m,a,i],       [o,j,l,e,k,g,d,m,e,h,k,r,j,j,l,k],       [e,c,o,h,a,n,f,k,d,q,k,k,a,j,f,p],       [v,l,i,q,p,p,k,o,m,e,d,l,g,m,k,s],       [e,n,i,g,e,q,l,m,i,o,g,m,i,c,l,k],       [q,n,j,q,l,h,f,o,b,j,p,c,l,l,u,t]]).


/*************main***************/
put_dominos:-
  dominos(D),
  length(D,N),
  length(Pos,N),
  make_domains(D,Pos,Domains),
  solve(Domains),
  frame(F),
  print_solution(Pos,F,1).
/********************************/

/**********make_domains**********/
make_domains([],_,[]).

make_domains([X|Dt],[Pos|T],[X-Pos-Dom|Doms]):-
  make_domains(Dt,T,Doms),
  make_domain(X,Dom).
/********************************/

/**********make_domain***********/
make_domain((A,A),Dom):-!,
  find_all_occurrences(A,[H|T]),
  asymmetrical_neighbors([H|T],T,[],Dom).

make_domain((A,B),Dom):-
  find_all_occurrences(A,Aocc),
  find_all_occurrences(B,Bocc),
  neighbors(Aocc,Bocc,Bocc,[],Dom).
/********************************/

/*******find_all_occurences******/
find_all_occurrences(X,Occ):-
  frame(F),
  find_in_frame(X,1,F,[],Occ).
/********************************/

/********find_in_frame***********/
find_in_frame(_,_,[],SFOcc,SFOcc).

find_in_frame(X,I,[Row|Rows],SFOcc1,Occ):-
  find_in_row(X,I,1,Row,SFOcc2),
  append(SFOcc1,SFOcc2,SFOcc3),
  I1 is I+1,
  find_in_frame(X,I1,Rows,SFOcc3,Occ).
/********************************/

/**********find_in_row***********/
find_in_row(_,_,_,[],[]).

find_in_row(X,I,J,[X|T],[(I,J)|T1]):- !,
  J1 is J+1,
  find_in_row(X,I,J1,T,T1).

find_in_row(X,I,J,[_|T],Occ):-
  J1 is J+1,
  find_in_row(X,I,J1,T,Occ).
/********************************/

/*****asymmetrical_neighbors*****/
asymmetrical_neighbors([],[],SFDom,SFDom).

asymmetrical_neighbors([_|Ta],[],SFDom1,Dom):-
  asymmetrical_neighbors(Ta,Ta,SFDom1,Dom).


asymmetrical_neighbors([(Ia,Ja)|Ta],[(Ib,Jb)|Tb],SFDom1,Dom):-
  (((Ia is Ib + 1; Ia is Ib - 1),Ja = Jb ; (Ja is Jb + 1; Ja is Jb - 1), Ia = Ib) ->
    append(SFDom1,[[(Ia,Ja),(Ib,Jb)]],SFDom2)
  ; 
    append(SFDom1,[],SFDom2)
  ),
  asymmetrical_neighbors([(Ia,Ja)|Ta],Tb,SFDom2,Dom).
/********************************/

/***********neighbors************/
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
/********************************/

/************solve***************/
solve([]).

solve(DomList):-
  mrv(DomList,D,Pos,Dom,Rest),
  member(Pos,Dom),
  update_domains(D,Pos,Rest,Updated),
  solve(Updated).
/********************************/

/**************mrv***************/
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
/********************************/

/********update_domains**********/
update_domains(_,_,[],[]).

update_domains(D,Pos,[D1-Pos1-Dom1|Doms],[D1-Pos1-UDom1|UDoms]):-
  update_domain(Pos,Dom1,[],UDom1),
  update_domains(D,Pos,Doms,UDoms).
/********************************/

/*********update_domain**********/
update_domain(_,[],SFNdom,SFNdom).

update_domain([(Ia,Ja),(Ib,Jb)],[Pos1|Rest],SFNdom,Ndom):-
  (member((Ia,Ja),Pos1) ; member((Ib,Jb),Pos1)) ->
    update_domain([(Ia,Ja),(Ib,Jb)],Rest,SFNdom,Ndom)
  ; 
    append(SFNdom,[Pos1],SFNdom1),
    update_domain([(Ia,Ja),(Ib,Jb)],Rest,SFNdom1,Ndom).
/********************************/

/*********print_solution*********/
print_solution(Pos,[Row],I):-!,
  print_row(Pos,Row,I,1,[],_),nl.

print_solution(Pos,[Row|Rows],I):-
  print_row(Pos,Row,I,1,[],NL),
  print_NextLine(NL),
  I1 is I+1,
  print_solution(Pos,Rows,I1).
/********************************/

/***********print_row************/
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
/********************************/

/**********get_paired************/
get_paired(X,[[X,Y]|_],Y):-!.

get_paired(X,[[Y,X]|_],Y):-!.

get_paired(X,[_|Rest],Y):-
  get_paired(X,Rest,Y).
/********************************/

/*********print_NextLine*********/
print_NextLine([X]):-!,
  writeln(X).

print_NextLine([X|T]):-
  write(X),write(' '),
  print_NextLine(T).
/********************************/
