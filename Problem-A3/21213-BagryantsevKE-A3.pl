/*
 * Let us now try to develop an interactive expert system utilizing hybrid chaining approach.
 * It means we start with no information at all about what user wants to know, and should
 * somehow pose hypothesis and get an information to work with.
 *
 * We also want our rules work not only with binary answers like 'yes' or 'no', but take
 * some values from user. The knowledge base we are about to implement consists of the
 * following IF-THEN rules:
 *
1.if guitars be distorted and drums be active then direction be rock.
2.if direction be rock and tempo be fast then style be hard_rock.
3.if style be hard_rock and lyrics be gangs then genre be british_rock.
4.if style be hard_rock and lyrics be party then genre be punk_rock.
5.if style be hard_rock and lyrics be fantasy then genre be rock_ballad.
6.if direction be rock and tempo be slow then style be soft_rock.
7.if style be soft_rock and lyrics be chill then genre be grunge.
8.if style be soft_rock and lyrics be love then genre be pop_rock.
9.if guitars be distorted and drums be blasting then direction be metal.
10.if direction be metal and tempo be fast then style be speed_metal.
11.if style be speed_metal and vocals be rap then genre be rapcore.
12.if style be speed_metal and vocals be scream then genre be trash_metal.
13.if direction be metal and tempo be slow then style be melodic_metal.
14.if style be melodic_metal and lyrics be love then genre be doom_metal.
15.if style be melodic_metal and lyrics be fantasy then genre be viking_metal.
16.if vocals be rap and drums be groovy then direction be hip_hop.
17.if direction be hip_hop and tempo be slow then style be melodic_rap.
18.if direction be hip_hop and tempo be fast then style be fast_flow.
19.if style be fast_flow and lyrics be party then genre be rave.
20.if style be fast_flow and lyrics be gangs then genre be drill.
21.if style be melodic_rap and lyrics be love then genre be hookah_rap.
22.if style be melodic_rap and lyrics be fantasy then genre be cloud_rap.
 *
 * As we see, some properties have binary values ('guitars' are expected to be distorted or not),
 * and some aren't of binary domain. For example, drums could either be groovy, blasting or active.
 * When asking user to provide a property's value we should at the same time show the corresponding domain.
 * For example if asking about drums:
 *
 * Of what type drums is ? Please choose one of the below:
 *  1. active
 *  2. blasting
 *  3. groovy
 *
 * We expect user to enter an integer from 1 to 3.
 *
 * Example:
 *
 * ?- start.
 * Hi!
 * 
 * Is it true that guitars is distorted ? Please answer 'yes', 'no' or 'why'.
 * |: no
 * 
 * Of what type vocals is? Type an integer or 'why'.
 * 1: scream
 * 2: rap
 * |: 2
 * 
 * Of what type drums is? Type an integer or 'why'.
 * 1: blasting
 * 2: groovy
 * 3: active
 * |: 2
 * 
 * Of what type tempo is? Type an integer or 'why'.
 * 1: slow
 * 2: fast
 * |: 1
 * 
 * Of what type lyrics is? Type an integer or 'why'.
 * 1: gangs
 * 2: love
 * 3: fantasy
 * 4: party
 * 5: chill
 * |: 2
 * Conclusion: genre be hookah_rap
 * Explanation: genre be hookah_rap<==(genre be hookah_rap<==style be melodic_rap and (lyrics be love<==is_stated))
 * true.
*/
:- use_module(library(clpfd)).

% As before we define dynamic procedures for data already derived, for questions already asked
% and for hypothesis already rejected.
:- dynamic derived/1.
:- dynamic asked/1.
:- dynamic rejected/1.

% Syntax of rules and proof trees.
:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

% Clear dynamic database on program start up.
:- 
    retractall(derived(_)),
    retractall(rejected(_)),
    retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% It would be handy to use binary predicate 'is' to denote properties' values, but,
% alas, 'is'/2 is a built-in ISO predicate, and it cannot be redefined. So we are forced
% to find an alternative. For example we can define a predicate 'be'/2. Using 'be' in rules
% makes them sound like being pronounced by some illiterate barbarians, but nothing better
% comes in mind.
:- op(100, xfx, be).

% Properties we can ask about. Each provided with its' domain. If domain consists of
% a single value then property is a binary one, otherwise we should show a menu.
askable(drums, [blasting, groovy, active]).
askable(guitars, [distorted]).
askable(vocals, [scream, rap]).
askable(tempo, [slow, fast]).
askable(lyrics, [gangs, love, fantasy, party, chill]).
askable(country, [russia, england, america]).

if
    guitars be distorted and drums be active
then
    direction be rock.
if
    direction be rock and tempo be fast
then
    style be hard_rock.
if
    style be hard_rock and lyrics be gangs
then
    genre be british_rock.
if
    style be hard_rock and lyrics be party
then
    genre be punk_rock.
if
    style be hard_rock and lyrics be fantasy
then
    genre be rock_ballad.
if
    direction be rock and tempo be slow
then
    style be soft_rock.
if
    style be soft_rock and lyrics be chill
then
    genre be grunge.
if
    style be soft_rock and lyrics be love
then
    genre be pop_rock.
if
    guitars be distorted and drums be blasting
then
    direction be metal.
if
    direction be metal and tempo be fast
then
    style be speed_metal.
if
    style be speed_metal and vocals be rap
then
    genre be rapcore.
if
    style be speed_metal and vocals be scream
then
    genre be trash_metal.
if
    direction be metal and tempo be slow
then
    style be melodic_metal.
if
    style be melodic_metal and lyrics be love
then
    genre be doom_metal.
if
    style be melodic_metal and lyrics be fantasy
then
    genre be viking_metal.
if
    vocals be rap and drums be groovy
then
    direction be hip_hop.
if
    direction be hip_hop and tempo be slow
then
    style be melodic_rap.
if
    direction be hip_hop and tempo be fast
then
    style be fast_flow.
if
    style be fast_flow and lyrics be party
then
    genre be rave.
if
    style be fast_flow and lyrics be gangs
then
    genre be drill.
if
    style be melodic_rap and lyrics be love
then
    genre be hookah_rap.
if
    style be melodic_rap and lyrics be fantasy
then
    genre be cloud_rap.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Backward chain inference engine. Looks familiar with what we already did. Note that here we
% work non only with binary rules, so we need to develop two ways of asking questions and processing
% answers.
true(Statement, Proof) :- 
                            retractall(derived(_)),
                            retractall(asked(_)),
                            retractall(rejected(_)),
                            true(Statement, Proof, []).

true(Statement, Statement, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
                                    true(S1, P1, Trace),
                                    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
                                    true(S1, P, Trace) ;
                                    true(S2, P, Trace).
true(Conclusion, Conclusion <== ConditionProof, Trace) :-
                                                        if Condition then Conclusion,
                                                        true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-
                                    Statement = Subject be _,
                                    askable(Subject, Menu),
                                    \+ derived(Statement),
                                    \+ asked(Subject),
                                    ask(Statement, Subject, Proof, Trace, Menu).

% Ask question in case of binary property. A property is detected to be binary if its' domain has only one value.
%
% ask(+Statement, +Subject, -Proof, -Trace, +Menu)
% Statement: current proposition we are trying to derive, asking a question. For example: whether guitars are distorted?.
% Subject  : subject of the proposition we ask about. For example if we are asking whether guitars are distorted, the subject
%            would be 'guitars'. When asking about a lyrics, lyrics is the subject.
% Proof    : Current proof tree.
% Trace    : Trace of rules we use to answer the WHY-question.
% Menu     : Subject's domain as a list. Domains are defined in predicate askable/2.
ask(Statement, Subject, Proof, Trace, [Val]) :-
                                format('\nIs it true that ~w is ~w ? Please answer \'yes\', \'no\' or \'why\'.\n',[Subject, Val]),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process_yes_no(Answer, Statement, Subject, Proof, Trace, [Val]).

% Same as previous but asking question about non-binary property. It requires showing menu.
ask(Statement, Subject, Proof, Trace, [V,V1|Vs]) :-
                                Menu = [V,V1|Vs],
                                format('\nOf what type ~w is? Type an integer or \'why\'.\n', [Subject]),
                                show_menu(1, Menu),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process(Answer, Statement, Subject, Proof, Trace, Menu).

% Shows menu as a enumerated list of values.
% For example:
%
% 1. Val1
% 2. Val2
% 3. Val3
show_menu(_, []).
show_menu(Counter, [V|Vs]) :-
                            format('~d: ~w\n', [Counter, V]),
                            Next #= Counter + 1,
                            show_menu(Next, Vs).

% Process non-binary answer.
%
% process(+Answer, +Statement, +Subject, -Proof, -Trace, +Menu)
% Answer   : an answer got from a user. It could be 'why' or some integer
% Statement: statement we are trying to derive
% Subject  : property we are asking about (nostrils, feet, wings etc).
% Proof    : Statement's proof tree
% Trace    : list of rules we are about to use to derive the Statement
% Menu     : Domain
process("why", St, S, Proof, Trace, Menu) :- !,
                                        show_reasoning_chain(Trace, 0), nl,
                                        ask(St, S, Proof, Trace, Menu).

% Note the last expression in the predicate. We should check if we derived what we are trying to derive.
% Let us suppose we want to confirm a hypothesis that direction is metal and to do so we ask of what type
% bill is. In order to derive that direction is metal we expect drums to be blasting, meaning that the
% Statement here is (drums be blasting). But user can choose any value from the menu. Let it be, for example,
% (drums be groovy). So, the Proposition we have just derived is (drums be groovy), and Statement we wanted to
% derive is (drums be blasting).
process(StrInd, St, S, Proof <== is_stated, _, Menu) :-
                                            atom_number(StrInd, Index),     % convert string to integer I
                                            nth1(Index, Menu, Answer),      % get I-th element from the domain if it exists
                                            !,
                                            Proposition = S be Answer,      % Make a proposition from user's answer
                                            Proof = Proposition,            % Proof of the proposition is proposition itself
                                            asserta(derived(Proposition)),  % Save the information that the proposition is derived
                                            asserta(asked(S)),              % Save the information that the question was asked
                                            St == Proposition.              % Check if we derived what we want to derive.

% Process incorrect answers.
process(_, Statement, Subject, Proof, Trace, Menu) :-
                                            write('Incorrect answer! Try again, please\n'),
                                            ask(Statement, Subject, Proof, Trace, Menu).

% Process binary answers. Nothing new here. It is the same as we did before.
process_yes_no("yes", S, Subj, S <== is_stated, _, _) :- !,
                                        asserta(derived(S)),
                                        asserta(asked(Subj)).
process_yes_no("no", _, S, _, _, _) :-   !,
                            asserta(asked(S)),
                            fail.
process_yes_no("why", Statement, Subject, Proof, Trace, Menu) :-  !,
                                            show_reasoning_chain(Trace, 0), nl,
                                            ask(Statement, Subject, Proof, Trace, Menu).
process_yes_no(_, Statement, Subject, Proof, Trace, Menu) :-
                        write('Please answer \'yes\', \'no\' or \'why\'!\n'),
                        read_string(user_input, "\n", "\r\t", _, Answer),
                        process_yes_no(Answer, Statement, Subject, Proof, Trace, Menu).


show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
                                                        format('\n   To infer ~w, using rule\n   (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        show_reasoning_chain(Rules, _).


% User interaction section.

% Check if some Conclusion is derivable.
%
% derivable(?Condition, -Conclusion, -ConclusionProofTree)
% Condition          : A piece of currently known information (could be unbound)
% Conclusion         : A hypothesis we want to confirm or reject, based on currently known information
% ConclusionProofTree: If hypothesis is confirmed ConclusionProofTree is a assigned with the corresponding proof tree
derivable(CondPart, Concl, Concl <== How) :-
                                if Cond then Concl,             % looking for a rule to apply
                                contains_term(CondPart, Cond),  % check if Condition occurs in the IF-path of the rule
                                \+ derived(Concl),
                                \+ rejected(Concl),
                                % If Concl is true then the hypothesis is confirmed, otherwise it is rejected
                                (
                                    true(Concl, How, []) -> !, asserta(derived(Concl)); asserta(rejected(Concl)), fail
                                ).

% infer(+CurrentCondition, -Hypothesis, +CurrentExplanation, -NextExplanation)
% CurrentCondition    : A piece of information we use to make a hypothesis
% Hypothesis          : A hypothesis we make based on the CurrentCondition
% CurrentExplanation  : Current proof tree
% NextExplanation     : A proof tree we will make in case the hypothesis is confirmed
infer(Cond, Concl, Prev, Expl) :-
                            derivable(Cond, Concl1, Trace),     % check if hypothesis is derivable
                            !,
                            infer(Concl1, Concl, Trace, Expl) ; % if so, use it as a new condition
                            Expl = Prev,                        % otherwise the final proof tree is equal to the current one
                            Concl = Cond.                       % and return the current Condition as the last information we were able to derive.


infer(Conclusion, Proof) :-
                            infer(_, Conclusion, 'Can\'t infer something from the information provided.' , Proof), !.



demo :-
            retractall(derived(_)),
            retractall(rejected(_)),
            retractall(asked(_)),
            write('Hi!\n'),
            infer(Conclusion, How),
            format('Conclusion: ~w\nExplanation: ~w', [Conclusion, How]).


clear :- retractall(derived(_)), retractall(rejected(_)), retractall(asked(_)).
