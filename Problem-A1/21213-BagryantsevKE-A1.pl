:- use_module(library(clpfd)).

author(olson).
author(hansen).
author(jones).
author(stout).

book(away).
book(dessert).
book(wendalissa).
book(call).

writer(Name, Book, Sale).

unique([]):-!.
unique([Head|Tail]):-
   member(Head, Tail), !, fail;
   unique(Tail).

solve(Solve, Solution):-
    Solve = [writer(X, XBook, XSale), writer(Y, YBook, YSale),
    writer(Z, ZBook, ZSale), writer(W, WBook, WSale)],
    
    Solution = [XAtom, YAtom, ZAtom, WAtom],
    when( nonvar(XTerm), term_to_atom(XAtom, XTerm) ),
    when( nonvar(YTerm), term_to_atom(YAtom, YTerm) ),
    when( nonvar(ZTerm), term_to_atom(ZAtom, ZTerm) ),
    when( nonvar(WTerm), term_to_atom(WAtom, WTerm) ),
    when( ( nonvar(XSale) , nonvar(X), nonvar(XBook) ), atomic_list_concat([XSale, X, XBook], '-', XTerm) ), 
    when( ( nonvar(YSale) , nonvar(Y), nonvar(YBook) ), atomic_list_concat([YSale, Y, YBook], '-', YTerm) ),
    when( ( nonvar(ZSale) , nonvar(Z), nonvar(ZBook) ), atomic_list_concat([ZSale, Z, ZBook], '-', ZTerm) ), 
    when( ( nonvar(WSale) , nonvar(W), nonvar(WBook) ), atomic_list_concat([WSale, W, WBook], '-', WTerm) ), 
    
    author(X), author(Y), author(Z), author(W), unique([X, Y, Z, W]),
    
    book(XBook), book(YBook),
    book(ZBook), book(WBook),
    unique([XBook, YBook, ZBook, WBook]),
    
%Tip #1
    ((member(writer(olson, dessert, _), Solve) , member(writer(_, wendalissa, 19), Solve)) ;
    (member(writer(olson, wendalissa, _), Solve) , member(writer(_, dessert, 19), Solve))),

%Tip #2
    ((member(writer(jones, _, 19), Solve) , member(writer(_, away, 33), Solve)) ;
    (member(writer(jones, _, 33), Solve) , member(writer(_, away, 19), Solve))),

%Tip #3
    ((member(writer(_, wendalissa, ASales), Solve) , member(writer(stout, _, BSales), Solve))), ASales #= BSales + 7.    

sol(Solution) :-
    solve(S, Solution), S = [writer(_, _, 19), writer(_, _, 33), writer(_, _, 12), writer(_, _, 26)].