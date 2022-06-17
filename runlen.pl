
decode_rl([],[]).

decode_rl([X|T],L3):-
  decode_rl(T,L1),
  decode_el(X,L2),
  append(L2,L1,L3).
    

decode_el(','(A,B),L):-
  nonvar(A),integer(B),
  gen_list(A,B,L).

decode_el(X,L):-
  nonvar(X),
  X \= ','(_,_),
  gen_list(X,1,L).

gen_list(_,0,[]).
gen_list(X,N,[X|L1]):-
  N > 0, N1 is N - 1, gen_list(X,N1,L1).


encode_rl([],[]).

encode_rl(L,EL1):-
  cons_pref(L,_,R,S),
  encode_rl(R,EL2),
  append([S],EL2,EL1).
  
  

cons_pref([X],[X],[],X).

cons_pref([X,X|T],[X|Pref],Rest,(X,N)):-
  cons_pref([X|T],Pref,Rest,_),!,
  length([X|Pref],N).

cons_pref([X,Y|T],[X],[Y|T],X).



/*
?- encode_rl([p(3), p(X), q(X), q(Y), q(4)], L).
X = 3
Y = 3
L = [(p(3), 2), (q(3), 2), q(4)]
Yes (0.00s cpu, solution 1, maybe more)
% More... %
No (0.00s cpu)
*/

/*                       ________
                        |Εξήγηση:|
                        **********
 Στην αρχή θα πάει να ενοποιηθεί το p(3) με το  p(X),
 αρα X = 3, βρίσκει το pref [p(3),p(3)] και κανει backtrack,
 έχει τώρα να ενοποιήσει q(3) = q(Y), άρα Y = 3,
 βρίσκει πάλι το [q(3),q(3)], και συνεχίζει για q(4), αφού
 q(3) δεν ενοποιείται με q(4), βρίσκει τελικά [q(4)]. 
 Μετά κάνει τα κατάλληλα encodings για κάθε pref που βρήκε
 και μέσω της append φτιάχνει την encoded λίστα. 
*/
