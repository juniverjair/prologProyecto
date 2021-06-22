%% Author: Javier Tibau

list([H|T],H,T).

%% Tokenizer code
%Created by Bruno Dufour, Fall 2005
%
% Append
append([ ],A,A).
append([A|B],C,[A|D]) :- append(B,C,D).

gather(Chars) --> [C],  {alphaNumeric(C)}, gather(Rest), {Chars=[C|Rest]}.

gather([]) --> {true}.
alphaNumeric(C):- 96<C,C<123;
                  64<C, C<91;
                  47<C, C<58.

% - Floats ---------------------------------------------------------------------
digit(D) --> [D], {47 < D, D < 58}.
nzdigit(D) --> [D], {48 < D, D < 58}.

floatlit(F) -->
        nzdigit(D0),
        digits(D1),
        ".",
        nedigits(D2),
        {append([D0|D1], [46], T), append(T, D2, D), name(F, D)}.

nedigits([D|T]) -->
        digit(D), !,
        digits(T).

digits(L) --> nedigits(L).
digits([]) --> [].
% ------------------------------------------------------------------------------

% - Strings --------------------------------------------------------------------

quote('"').

gatherString(Chars) --> [C], {C=\=34}, gatherString(Rest), {Chars=[C|Rest]}.
gatherString([]) --> {true}.

stringlit(S) --> "\"", gatherString(Chars), "\"", {string_to_list(S,Chars)}.

% ------------------------------------------------------------------------------

% Tokeinze comparison operators
tokenize(Z) --> "==", tokenize(Y), {Z = [== | Y]}.
tokenize(Z) --> ">=", tokenize(Y), {Z = [>= | Y]}.
tokenize(Z) --> "<=", tokenize(Y), {Z = [<= | Y]}.
tokenize(Z) --> "<>", tokenize(Y), {Z = [<> | Y]}.
tokenize(Z) --> ">", tokenize(Y), {Z = [> | Y]}.
tokenize(Z) --> "<", tokenize(Y), {Z = [< | Y]}.

% Tokenize float
tokenize(Result) --> floatlit(F), tokenize(Rest), {Result=[F|Rest]}.
% Tokenize string
tokenize(Result) --> stringlit(S), tokenize(Rest), {Result=[S|Rest]}.
% Tokenize id / int
tokenize(Result) --> gather(Chars),{\+ Chars =[]},tokenize(RestResult),
                    {name(N,Chars), Result=[N|RestResult]}.
% Discard whitespace
tokenize(R)-->[C],{C<33},tokenize(R).
% Tokenize special character
tokenize([N|R]) --> [C],{C>32},
                        {name(N,[C])},tokenize(R).
tokenize([])-->[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Listas de una en 1D en JavaScript (Modo no estricto)

% Operadores de primera ejecucion
oper1(+).
oper1(-).

% Operadores de segunda ejecucion
oper2(*).
oper2(/).

% <igual> --> =
igual(=).

% <type> --> <var> | <let> | <cons>
type(var).
type(let).
type(cons).

% <asig> --> <atom> \+ <ops>
ops(X):- igual(X) | oper1(X) | oper2(X) | type(X).
asig(ID):- atom(ID) | \+ ops(ID).

% <sep> --> ,
sep(,).

% <dato> --> <decimal> | <cadena> | <entero> | <identificador>
dato([X|TSFinal],TSFinal):- asig(X) | float(X) | string(X) | integer(X).

% <expresion2> --> <id> | <decimal> | <cadena> | <entero> | <identificador> 
expresion2(TSFinal_X,TSFinal):- dato(TSFinal_X,TSFinal).
expresion2(['('|TSInicial], TSFinal):- dato(TSInicial, [ ')' | TSFinal ]).

% <expresion1> --> <expresion1> <sep> <expresion2> | <expresion2>
expresion1(TSInicial,TSFinal):- expresion2(TSInicial,[S|TSFinal_X]), sep(S), expresion1(TSFinal_X,TSFinal).
expresion1(TSInicial,TSFinal):- expresion2(TSInicial,TSFinal).

% <expresion> --> <expresion> <sep> <expresion1> | <expresion1>
expresion(TSInicial,TSFinal):- expresion1(TSInicial,[S|TSFinal_X]), sep(S), expresion(TSFinal_X,TSFinal).
expresion(TSInicial,TSFinal):- expresion1(TSInicial,TSFinal).

% <stmt> --> <type> <asig> = <expresion>;
stmt([T,X,I|TSInicial],TSFinal):- type(T), asig(X), igual(I), expresion(['[]'|TSInicial],[';'|TSFinal]).

% Ejecuta el programa
parseTree(FileName):-
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSInicial), ProgramString),
    write('TSInicial:'),writeln(TSInicial),
    stmt(TSInicial, []).
