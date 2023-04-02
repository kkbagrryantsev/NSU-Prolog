:-
prolog_load_context(directory,Dir),
atom_concat(Dir, "./wordnet", WNDB_),
absolute_file_name(WNDB_, WNDB),
asserta(user:file_search_path(wndb, WNDB)).
:- use_module("./wnload/prolog/wn").
:- use_module(library(clpfd)).

:- op(400, xfy, wn_hyp).
:- op(400, xfy, wn_mm).
:- op(400, xfy, wn_mp).

test(Connection) :- related_words(mouse/PoS1/Sense1/Syn1, squirrel/PoS2/Sense2/Syn2, 5, Connection).

related_words(WordA/PA/SA/SynA, WordB/PB/SB/SynB, Depth, Connection) :-
    word_getter((SynA/WordA/PA/SA),
                (SynB/WordB/PB/SB)),
    Possible_Depth #=< Depth-1, 
    Depth-1 #> 0,
    relation_chain(SynA, SynB, [], Synsets, Possible_Depth),
    format_syncet(Synsets, Connection).

related_synsets(SynA, hyp, SynB) :-
    SynA wn_hyp SynB ; SynB wn_hyp SynA.

related_synsets(SynA, mm, SynB) :-
    SynA wn_mm SynB ; SynB wn_mm SynA.

related_synsets(SynA, mp, SynB) :-
    SynA wn_mp SynB ; SynB wn_mp SynA.

relation_chain(SynA, SynB, Acc, [(SynA/Rel/SynB)], Depth) :- 
    Depth #= 0,
    related_synsets(SynA, Rel, SynB),
    \+member((SynA/Rel/SynB), Acc),
    \+member((SynB/Rel/SynA), Acc).

relation_chain(SynA, SynB, Acc, [(SynA/Rel/SynX)|Path], Depth) :- 
    Depth #> 0, 
    related_synsets(SynA, Rel, SynX),
    \+member((SynA/Rel/SynX), Acc),
    \+member((SynX/Rel/SynA), Acc),
    relation_chain(SynX, SynB, [(SynA/Rel/SynX)|Acc], Path, Depth-1).

word_getter((SynA/WordA/PA/SA), (SynB/WordB/PB/SB)) :-
    wn_s(SynA, _, WordA, PA, SA, _),
    wn_s(SynB, _, WordB, PB, SB, _).

format_syncet([(SynA/Relation/SynB)|[]], [r(WordA/PA/SA, Relation, WordB/PB/SB)|[]]) :-
    word_getter((SynA/WordA/PA/SA),
                (SynB/WordB/PB/SB)).

format_syncet([(SynA/Relation/SynB)|Acc], [r(WordA/PA/SA, Relation, WordB/PB/SB)|Connection]) :-
    word_getter((SynA/WordA/PA/SA),
                (SynB/WordB/PB/SB)),
    format_syncet(Acc, Connection).
