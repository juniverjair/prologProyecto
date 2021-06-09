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

% Operadores de primera ejecucion
oper1(+).
oper1(-).

% Operadores de segunda ejecucion
oper2(*).
oper2(/).

% Definicion  <Var> negacion de <oper1>
asig(X):- atom(X), \+ oper1(X).

% <expresion> -->  <identificador>
expresion([X|TSFinal],TSFinal):- asig(X).

% <expresion> --> <decimal>
expresion([X|TSFinal],TSFinal):- float(X).

% <expresion> -->  <cadena> 
expresion([X|TSFinal],TSFinal):- string(X).

% <expresion> -->  <entero> 
expresion([X|TSFinal],TSFinal):- integer(X).

% <expresion> --> <oper1> <expresion2>
expresion([X|TSFinalX],TSFinal):- oper1(X), expresion2(TSFinalX,TSFinal).

% <expresion> --> (<expresion2>)
expresion(['('|TSInicial], TSFinal ):- expresion2(TSInicial, [ ')' | TSFinal ]).

% <expresion1> --> <expresion> <oper2> <expresion1>
expresion1(TSInicial,TSFinal):- expresion(TSInicial,[O|TSFinalX]), oper2(O), expresion1(TSFinalX,TSFinal).

% <expresion1> --> <expresion>
expresion1(TSInicial,TSFinal):- expresion(TSInicial,TSFinal).

% <expresion2> --> <expresion1> <oper1> <expresion2>
expresion2(TSInicial,TSFinal):- expresion1(TSInicial,[O|TSFinalX]), oper1(O), expresion2(TSFinalX,TSFinal).

% <expresion2> -->  <expresion1>
expresion2(TSInicial,TSFinal):- expresion1(TSInicial,TSFinal).

% <asignacion> --> <asig> = <expresion2>
asignacion([X,=|TSInicialNoX],TSFinal):- asig(X), expresion2(TSInicialNoX,TSFinal).

% Ejecuta el programa
parseTree(FileName):-
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSInicial), ProgramString),
    asignacion(TSInicial, []).
