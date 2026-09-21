__INCLUDE_HEAP_DEFS__

empty([]).

allocated(X, [X|_]):-!.
allocated(X, [_|T]):-allocated(X,T).

remove([], _, []).
remove([Node|T], Node, OutSet) :- !, remove(T, Node, OutSet).
remove([Head|T], Node, [Head|OutSet]) :- remove(T, Node, OutSet).

eq(X,X).
neq(X,Y) :- X \= Y.
