-module(zadanie3).
-export([start/2, ping/2 ]).



start(N, X) -> Pn = spawn(zadanie3, ping, [tba,1]), start(N, X, Pn, Pn).

start(1, X, Pp, Pn) -> Pn ! {go, Pp}, Pn ! {f,X}, done;
start(N, X, Pp, Pn) -> P = spawn(zadanie3, ping, [Pp,N]), start(N-1, X, P, Pn).



ping(tba,Numer) -> receive {go,P} -> ping(P,Numer) end;
ping(P,Numer) -> receive
				{f,X} -> 
						 if 
						 
								Numer rem 2 == 0 -> io:format("~p ~p ~n", [Numer,X]), P !{f, X*2};
								Numer rem 2 == 1 -> io:format("~p ~p ~n", [Numer,X]), P ! {f, X-1}
								end,
								
						ping(P,Numer)
			end.
				

