% Definiciones de operadores.
:- op(900,xfy, [ + ]).
:- op(800,xfy, [ * ]).

% Implementación de los predicados.

% acciones(+Proceso, -Acciones).
acciones(0,[]).
acciones(tau * P, A):- acciones(P,A).
acciones(M * P, [M|A]):- M \= tau, acciones(P,A).
acciones(P + Q, A):-
  acciones(P, PA),
  acciones(Q, QA),
  append(PA, QA, REPA),
  list_to_set(REPA, A).

% reduce(+Proceso1,?Accion,?Proceso2)
reduce(A*P1,A,P1).
reduce(P+_,A,R):- reduce(P,A,R).
reduce(_+Q,A,R):- reduce(Q,A,R).

% reduceLista(+Proceso1,?Cadena,?Proceso2)
reduceLista(P1, [], P2):- P1 = P2.
reduceLista(P1,[A|AS],P3):-
  reduce(P1,A,P2),
  A \= tau,
  reduceLista(P2,AS,P3).
reduceLista(P1,AS,P3):-
  reduce(P1,tau,P2),
  reduceLista(P2,AS,P3).


% trazas(+Proceso, -Cadenas)
trazas(0,[[]]).
trazas(tau*P, T):-  trazas(P,T).
trazas(M*0,N):- M\= tau , N=[[],[M]].
trazas(M*P, [[]|T]):-   M\= tau , P\=0, trazas(P,T2), map_elem(T2, M, T).
trazas(P+Q,T):- trazas(P,TP) , trazas(Q, TQ), append(TP, TQ, TPQ), list_to_set(TPQ, T).

% map_elem(?T2, ?M, ?T).
map_elem([[]],M,[[M]]).
map_elem([[X|XS]],M, [[M,X|XS]]).
map_elem([X|YS], M, [[M|X]|T]):- map_elem(YS,M,T).



% Tests (van un par de ejemplos, agreguen los suyos).

%test(0) :- not((acciones(0, L), member(_,L))).

%test(1) :- reduceLista(0,[],0).

%test(2) :- not(puedeReemplazarA(moneda * (te * 0 + cafe * 0), moneda * te * 0 + moneda * cafe * 0)).
%test(3) :- puedeReemplazarA(tau*a*0, a*0).

%test(4) :- equivalentes(a*b*(c*0+d*0), a*b*(d*tau*0+c*0)).
%test(5) :- not(equivalentes(a*b*0, b*a*0)).

%tests :- forall(between(0, 5, N), test(N)). %Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.
