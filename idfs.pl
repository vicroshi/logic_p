idfs(ILstate,FLstate,FLstate,IMstate,IMstate,_).

idfs(ILstate,FLstate,Lstate,IMstate,FMstate,D):-
  move(ILstate,FLstate,Lstate,Mstate),
  append(IMstate, Mstate, SFMstate),
  D1 is D-1,
  idfs(ILstate,FLstate,Lstate,SFMstate,FMstate,D1).



  

