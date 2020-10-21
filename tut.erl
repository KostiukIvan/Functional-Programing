-module(tut).
-export([double/1 , fac/1, start/1, ping/1, format_list/1, fnl/1, sumList/1, start4/2, ping4/1, start5/1, pr1/0, pr2/0, pr3/0]).

%Zadanie1
double(X) ->
    2 * X.

%Zadanie2	
fac(0) ->
	1;
fac(N) -> 
	N * fac(N-1).
	
%Zadanie3 16-17
start(N) -> Pn = spawn (tut, ping, [tba]), start(N-1, Pn, Pn).

start(0, Pp, Pn) -> Pn ! {go, Pp}, Pn ! {f,1}, done;
start(N, Pp, Pn) -> P = spawn (tut, ping, [Pp]), start(N-1, P, Pn).

ping(tba) -> receive {go, P} -> ping(P)	end;		
ping(P) -> receive 
				{f,X} -> io:format("~p", [X]),
						P ! {s,[(X+1),X]},
						ping(P); 
				{s,[F|R]} -> format_list([F|R]), 
						P ! {s,[(F+1),F|R]},
						ping(P)
			end.
			
			
format_list(L) when list(L) ->
        io:format("["),
        fnl(L),
        io:format("]~n").

fnl([H]) ->
	io:format("~p", [H]);
fnl([H|T]) ->
	io:format("~p,", [H]),
	fnl(T);
fnl([]) ->
	ok.
	
	
	
%Zadanie4 16-17


start4(N,K) -> Pn = spawn(tut, ping4, [tba]), start4(N-1, K, Pn, Pn).

start4(0, K, Pp, Pn) -> Pn ! {go, Pp}, Pn ! {f,K}, done;
start4(N, K, Pp, Pn) -> P = spawn(tut, ping4, [Pp]), start4(N-1, K, P, Pn).



ping4(tba) -> receive {go, P} -> ping4(P) end;
ping4(P) -> receive
				{f,X} -> io:format("~p", [X]),
						P ! {s,[X,X]},
						ping4(P); 
				{s,[F|R]} -> format_list([F|R]),
						P ! {s,[sumList([F|R]),F|R]},
						ping4(P)
			end.
				

sumList([H]) -> H;
sumList([H|T]) -> H + sumList(T);
sumList([]) -> 0.


%Zadanie 17-18
start5(N) ->
	register(proc1, spawn(tut, pr1, [])),
	register(proc2, spawn(tut, pr2, [])),
	register(proc3, spawn(tut, pr3, [])),
	proc1 ! N,
	ok.

pr1() ->
	receive N -> io:format("p1 ~w~n", [N]), proc2 ! (N+1) end,
	pr1().

pr2() ->
	receive N -> io:format("p1 ~w~n", [N]), proc3 ! (N*2) end,
	pr2().
	
pr3() ->
	receive N -> io:format("p1 ~w~n", [N]), proc1 ! (N-3) end,
	pr3().
		
	



