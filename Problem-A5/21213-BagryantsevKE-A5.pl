:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, chars).

% Separators
separator --> ",".
separator --> "\s".
separator --> "\t".
separator --> "\n".
separators --> separator.
separators --> separator, separators.

% Special symbols
special_symbol --> "+".
special_symbol --> "-".
special_symbol --> ">".
special_symbol --> "<".
special_symbol --> "=".
special_symbol --> "*".
special_symbol --> "_".

% Integers
digit --> {char_type(D, digit)}, [D].
number --> "+", signed_number.
number --> "-", signed_number.
number --> digit, signed_number.
signed_number --> "".
signed_number --> digit, signed_number.

% Characters
character --> {char_type(C, alpha)}, [C].
any_key --> {char_type(A, print)}, [A].

% Strings
string --> ['"'], characters, ['"'].
characters --> "".
characters --> any_key, characters.

% Identificators
identificator --> character, suffix(letter).
identificator --> special_symbol, suffix(s_symbol).

% Identificator's suffix
suffix(letter) --> "".
suffix(letter) --> digit, suffix(letter).
suffix(letter) --> special_symbol, suffix(letter).
suffix(letter) --> character, suffix(letter).
suffix(s_symbol) --> "".
suffix(s_symbol) --> digit, suffix(s_symbol).
suffix(s_symbol) --> special_symbol, suffix(s_symbol).

% Keywords
keyword --> ":", character, suffix(letter).

% Atoms
atom --> string.
atom --> keyword.
atom --> identificator.
atom --> number.

% Sequences of atoms within parentheses
monomes --> s_expression.
monomes --> s_expression, separators, monomes.

% Seqeunces of even amount of atoms within curled brackets
curled_monomes(0) --> "".
curled_monomes(1) --> s_expression.
curled_monomes(N) --> s_expression, separators, {N #= M + 1}, curled_monomes(M).

% Clojure grammar
s_expression --> atom.
s_expression --> "(", monomes, ")", !.
s_expression --> "[", monomes, "]", !.
s_expression --> "{", {N #>= 0, N mod 2 #= 0}, curled_monomes(N), "}", !.

% Autotests
% Requires tests filepath. File must contain tests separated with newline
autotest(FilePath) :-
    see(FilePath),
    file2string(File),
    split_string(File, ".", "", SplittedString),
    maplist(test, SplittedString),
    seen().

file2string(Str):- get_code(H),
    (
        code_type(H, end_of_file), !, Str = []
        ;
        Str = [H|T], file2string(T)
    ).

rec(G, W) :- string_chars(W, Chars), call(G, Chars, []).

test(Expression) :-
    rec(s_expression, Expression),
    format('VALID expression:\n~w\n',[Expression]).
test(Expression) :-
    \+rec(s_expression, Expression),
    format('INVALID expression:\n~w\n',[Expression]).