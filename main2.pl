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

%% Listas de una en 1D en   avaScript (Modo no estricto)

% Operadores de primera ejecucion
oper1(+).
oper1(-).

% Operadores de segunda ejecucion
oper2(*).
oper2(/).

% <igual> --> =
igual(=).

% <comp> --> == | < | > | <= | >= 
comp(==).
comp(>=).
comp(<=).
comp(>).
comp(<).

% <type> --> <var> | <let> | <cons>
type(var).
type(let).
type(cons).

% <asig> --> <atom> \+ <ops>
ops(X):- igual(X) | oper1(X) | oper2(X) | type(X).
asig(ID):- atom(ID) | \+ ops(ID).

% <dato> --> <decimal> | <cadena> | <entero> | <identificador>
dato(X):- float(X) | string(X) | integer(X) | asig(X).

% <listaDatos> --> <dato> , <listaDatos>
listdatos([X,','|TSInicial],TSFinal):- dato(X),listdatos(TSInicial,TSFinal).

% <listaDatos> --> <dato> 
listdatos([X|TSFinal],TSFinal):- dato(X).

% <list> --> <type> <asig> = [ <listaDatos> ];
listStmt([T,X,I,'['|TSInicial],TSFinal):- type(T), asig(X), igual(I), listdatos(TSInicial,[']',';'|TSFinal]).

% <function> --> function
function(function).

% <funcion>--> function <atom> ( <listadatos> ) { }
funcionStmt([F,N,'('|TSInicial],TSFinal):- function(F),atom(N),listdatos(TSInicial,[')','{','}'|TSFinal]).

% <expr2> --> <id> | <integer> | <numWDecimal> | <stringLiteral> | (<expr>) 
expr2([X|TSEnd_I],TSEnd):-      oper1(X),expr(TSEnd_I,TSEnd).
expr2([X|TSEnd],TSEnd):-        integer(X).
expr2([X|TSEnd],TSEnd):-        float(X).
expr2([X|TSEnd],TSEnd):-        string(X).
expr2([X|TSEnd],TSEnd):-        asig(X).
expr2(['('|TSInit], TSEnd ):-   expr(TSInit, [ ')' | TSEnd ]).

% <expr1> --> <expr1> <op2> <expr2> | <expr2>
expr1(TSInit,TSEnd):- expr2(TSInit,[OP|TSEnd_I]), oper2(OP), expr1(TSEnd_I,TSEnd).
expr1(TSInit,TSEnd):- expr2(TSInit,TSEnd).

% <expr> --> <expr> <op1> <expr1> | <expr1>
expr(TSInit,TSEnd):- expr1(TSInit,[OP|TSEnd_I]), oper1(OP), expr(TSEnd_I,TSEnd).
expr(TSInit,TSEnd):- expr1(TSInit,TSEnd).
condExpr(TSInit,TSEnd):- expr(TSInit,[X|TSEnd_I]),comp(X),expr(TSEnd_I,TSEnd).

% <ifStmt> --> if (<condExpr>) {<listStmt>} else {<listStmt>}
ifStmt(['if','('|TSInicial],TSFinal):- condExpr(TSInicial,[')','{','}','else','{','}'|TSFinal]).
ifStmt(['if','('|TSInicial],TSFinal):- condExpr(TSInicial,[')','{','}'|TSFinal]).

% <whileStmnt> --> while (<condExpr>) {<listStmt>}
whileStmt(['while','('|TSInicial],TSFinal):- condExpr(TSInicial,[')','{','}'|TSFinal]).

%stmt(TSInit,TSEnd) :- forStmt(TSInit,TSEnd).
stmt(TSInit,TSEnd) :- ifStmt(TSInit,TSEnd).
stmt(TSInit,TSEnd) :- whileStmt(TSInit,TSEnd).
stmt(TSInit,TSEnd) :- funcionStmt(TSInit,TSEnd).
stmt(TSInit,TSEnd) :- listStmt(TSInit,TSEnd).

% Ejecuta el programa
parseTree(FileName):-
    open(FileName, 'read', InputStream),
    read_stream_to_codes(InputStream, ProgramString),
    close(InputStream),
    phrase(tokenize(TSInicial), ProgramString),
    % write('TSInicial:'),write(TSInicial),
    stmt(TSInicial, []).
