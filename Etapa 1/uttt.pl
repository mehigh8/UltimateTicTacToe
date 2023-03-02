:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.

% Am reprezentat o stare prin pozitia urmatoarei mutari (all daca poate pune oriunde),
% urmata de 9 liste, fiecare reprezentand cate o tabla.
initialState([all, B, B, B, B, B, B, B, B, B]) :- empty_board(B).

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards([_|Bs], Bs).

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(S, UPos, B) :- positions(P), nth0(N, P, UPos, _), getBoards(S, Bs), nth0(N, Bs, B, _).

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).

% count(+List, +Elem, -Times)
% Intoarce numarul de aparitii ale lui Elem in List.
count([], _, 0).
count([E|T], E, N) :- count(T, E, Nt), N is 1 + Nt.
count([H|T], E, N) :- H \= E, count(T, E, N).

% whoWon(+Board, -Winner)
% Intoarce castigatorul (daca exista) board-ului. Altfel, intoarce remiza sau ''.
whoWon(B, x) :- player_wins(x, B).
whoWon(B, 0) :- player_wins(0, B).
whoWon(B, r) :- \+player_wins(x, B), \+player_wins(0, B), count(B, '', 0).
whoWon(B, '') :- \+whoWon(B, x), \+whoWon(B, 0), \+whoWon(B, r).


getUBoard(State, US) :- getBoard(State, nw, NW), whoWon(NW, NWR),
						getBoard(State, n, N), whoWon(N, NR),
						getBoard(State, ne, NE), whoWon(NE, NER),
						getBoard(State, w, W), whoWon(W, WR),
						getBoard(State, c, C), whoWon(C, CR),
						getBoard(State, e, E), whoWon(E, ER),
						getBoard(State, sw, SW), whoWon(SW, SWR),
						getBoard(State, s, S), whoWon(S, SR),
						getBoard(State, se, SE), whoWon(SE, SER),
						US = [NWR, NR, NER, WR, CR, ER, SWR, SR, SER].

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(S, Up, P, C) :- getBoard(S, Up, B), getPos(B, P, C).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(B, Pos, C) :- positions(P), nth0(N, P, Pos, _), nth0(N, B, C, _).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(State, x) :- getBoard(State, nw, NW), count(NW, x, NW_X), count(NW, 0, NW_0),
						   getBoard(State, n, N), count(N, x, N_X), count(N, 0, N_0),
						   getBoard(State, ne, NE), count(NE, x, NE_X), count(NE, 0, NE_0),
						   getBoard(State, w, W), count(W, x, W_X), count(W, 0, W_0),
						   getBoard(State, c, C), count(C, x, C_X), count(C, 0, C_0),
						   getBoard(State, e, E), count(E, x, E_X), count(E, 0, E_0),
						   getBoard(State, sw, SW), count(SW, x, SW_X), count(SW, 0, SW_0),
						   getBoard(State, s, S), count(S, x, S_X), count(S, 0, S_0),
						   getBoard(State, se, SE), count(SE, x, SE_X), count(SE, 0, SE_0),
						   Xs is NW_X+N_X+NE_X+W_X+C_X+E_X+SW_X+S_X+SE_X,
						   Zeroes is NW_0+N_0+NE_0+W_0+C_0+E_0+SW_0+S_0+SE_0,
						   Xs =:= Zeroes.
getNextPlayer(State, 0) :- getBoard(State, nw, NW), count(NW, x, NW_X), count(NW, 0, NW_0),
						   getBoard(State, n, N), count(N, x, N_X), count(N, 0, N_0),
						   getBoard(State, ne, NE), count(NE, x, NE_X), count(NE, 0, NE_0),
						   getBoard(State, w, W), count(W, x, W_X), count(W, 0, W_0),
						   getBoard(State, c, C), count(C, x, C_X), count(C, 0, C_0),
						   getBoard(State, e, E), count(E, x, E_X), count(E, 0, E_0),
						   getBoard(State, sw, SW), count(SW, x, SW_X), count(SW, 0, SW_0),
						   getBoard(State, s, S), count(S, x, S_X), count(S, 0, S_0),
						   getBoard(State, se, SE), count(SE, x, SE_X), count(SE, 0, SE_0),
						   Xs is NW_X+N_X+NE_X+W_X+C_X+E_X+SW_X+S_X+SE_X,
						   Zeroes is NW_0+N_0+NE_0+W_0+C_0+E_0+SW_0+S_0+SE_0,
						   Xs =\= Zeroes.

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.

% getUnfinishedBoards(+State, +Pos, -Final)
% Creeaza o lista cu toate pozitiile tablelor care inca nu au un castigator.
getUnfinishedBoards(_, [], []).
getUnfinishedBoards(S, [H|T], F) :- getBoard(S, H, B), whoWon(B, ''), getUnfinishedBoards(S, T, Ft), F = [H|Ft].
getUnfinishedBoards(S, [H|T], F) :- getBoard(S, H, B), whoWon(B, W), W \= '', getUnfinishedBoards(S, T, F).

getNextAvailableBoards([Pp|_], Nbp) :- Pp \= all, Nbp = [Pp].
getNextAvailableBoards([all|Bs], Nbp) :- positions(P), getUnfinishedBoards([all|Bs], P, Nbp).

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(B, R) :- whoWon(B, R).

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s a realizat ultima mutare.
buildState(Bs, Pp, [Pp|Bs]) :- getBoard([Pp|Bs], Pp, B), whoWon(B, W), W = ''.
buildState(Bs, Pp, [all|Bs]) :- getBoard([all|Bs], Pp, B), whoWon(B, W), W \= ''.

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.

% finishedGame(+State)
% Verifica daca jocul de UTTT are deja un castigator.
finishedGame(S) :- getUBoard(S, Ub), whoWon(Ub, W), W \= ''.

validMove([UPos|Bs], M) :- UPos \= all, \+finishedGame([UPos|Bs]), getBoard([UPos|Bs], UPos, B), getPos(B, M, '').
validMove([all|Bs], (UPos, Pos)) :- \+finishedGame([all|Bs]), getUBoard([all|Bs], Ub), getPos(Ub, UPos, ''),
									getBoard([all|Bs], UPos, B), getPos(B, Pos, '').

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.

% editList(+List, +Elem, +Pos, +CurrPos, -NewList)
% Schimba elementul de pe pozitia Pos din lista List in Elem, rezultatul se afla in NewList.
editList([], _, _, _, []).
editList([_|T], Elem, Pos, CurrPos, Nl) :- CurrPos =:= Pos, NewPos is CurrPos + 1, editList(T, Elem, Pos, NewPos, Nlt), Nl = [Elem|Nlt].
editList([H|T], Elem, Pos, CurrPos, Nl) :- CurrPos =\= Pos, NewPos is CurrPos + 1, editList(T, Elem, Pos, NewPos, Nlt), Nl = [H|Nlt].

makeMove([UPos|Bs], Pos, Ns) :- UPos \= all, validMove([UPos|Bs], Pos),
								getBoard([UPos|Bs], UPos, B), getNextPlayer([UPos|Bs], Player),
								positions(P), nth0(Np, P, Pos, _), editList(B, Player, Np, 0, Nb),
								nth0(Nu, P, UPos, _), editList(Bs, Nb, Nu, 0, Nbs), buildState(Nbs, Pos, Ns).
makeMove([all|Bs], (UPos, Pos), Ns) :- validMove([all|Bs], (UPos, Pos)), getBoard([all|Bs], UPos, B),
									   getNextPlayer([all|Bs], Player), positions(P), nth0(Np, P, Pos, _),
									   editList(B, Player, Np, 0, Nb), nth0(Nu, P, UPos, _), editList(Bs, Nb, Nu, 0, Nbs),
									   buildState(Nbs, Pos, Ns).

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).

% getFirstMove(+Board, +Positions, -Pos)
% Intoarce prima pozitie din Board care nu este ocupata.
getFirstMove(B, [H|_], H) :- getPos(B, H, '').
getFirstMove(B, [H|T], Pos) :- getPos(B, H, R), R \= '', getFirstMove(B, T, Pos).

dummy_first(S, Pos) :- getNextAvailableBoards(S, [M|Ms]), length([M|Ms], 1), getBoard(S, M, B), positions(P), getFirstMove(B, P, Pos).
dummy_first(S, (UPos, Pos)) :- getNextAvailableBoards(S, Nab), length(Nab, L), L > 1,
							   getUBoard(S, Ub), getFirstMove(Ub, Nab, UPos), getBoard(S, UPos, B), positions(P), getFirstMove(B, P, Pos).

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).

% getLastMove(+Board, +Positions, -Pos)
% Intoarce ultima pozitie din Board care nu este ocupata.
getLastMove(B, P, Pos) :- reverse(P, Rp), getFirstMove(B, Rp, Pos).

dummy_last(S, Pos) :- getNextAvailableBoards(S, [M|Ms]), length([M|Ms], 1), getBoard(S, M, B), positions(P), getLastMove(B, P, Pos).
dummy_last(S, (UPos, Pos)) :- getNextAvailableBoards(S, Nab), length(Nab, L), L > 1,
							  getUBoard(S, Ub), getLastMove(Ub, Nab, UPos), getBoard(S, UPos, B), positions(P), getLastMove(B, P, Pos).
