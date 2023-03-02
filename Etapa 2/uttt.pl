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


% ======== Etapa 2

% movePriority/4
% movePriority(+Player, +Board, +Move, -Priority)
% Calculează prioritatea mutării Move pentru jucătorul Player, într-o
% tablă individuală Board. Vezi enunț.
movePriority(P, B, M, 0) :- positions(Pos), nth0(N, Pos, M, _), editList(B, P, N, 0, Nb), player_wins(P, Nb).
movePriority(P, B, M, 1) :- nextPlayer(P, Opp), movePriority(Opp, B, M, 0).
movePriority(_, B, M, 2) :- empty_board(B), member(M, [nw, ne, sw, se]).
movePriority(P, B, M, 3) :- count(B, P, 0), nextPlayer(P, Opp), getPos(B, c, Opp), member(M, [nw, ne, sw, se]).
movePriority(P, B, c, 3) :- count(B, P, 0), getPos(B, c, '').
movePriority(P, B, M, 4) :- positions(Pos), nth0(N, Pos, M, _), editList(B, P, N, 0, Nb), canWin(P, Nb, Pos).
movePriority(_, _, M, 5) :- member(M, [nw, ne, sw, se]).
movePriority(_, _, _, 6).

% canWin(+Player, +Board, +Positions)
% Verifica daca jucatorul Player poate castiga intr-o singură tura.
canWin(_, _, []) :- false.
canWin(P, B, [H|T]) :- getPos(B, H, ''), movePriority(P, B, H, 0); canWin(P, B, T).

% bestIndividualMoves/3
% bestIndividualMoves(+P, +Board, -Moves)
% Leagă Moves la o listă cu toate mutările disponibile, în ordinea
% priorității lor.
%
% Hint: construiți o listă de perechi (prioritate, mutare) și folosiți
% sortMoves/2 pentru a obține lista de mutări, în ordinea priorității.
bestIndividualMoves(P, B, Ms) :- positions(Pos), constructMoves(P, B, Pos, Ums), sortMoves(Ums, Ms).

% constructMoves(+Player, +Board, +Positions, -UnorderedMoves)
% Construieste perechile (prioritate, mutare) pentru jucatorul Player.
constructMoves(_, _, [], []).
constructMoves(P, B, [H|T], Ums) :- getPos(B, H, ''), movePriority(P, B, H, Pn), constructMoves(P, B, T, Tums), Ums = [(Pn, H)|Tums].
constructMoves(P, B, [H|T], Ums) :- \+getPos(B, H, ''), constructMoves(P, B, T, Ums).

% narrowGreedy/2
% narrowGreedy(+State, -Move)
% Strategie care întotdeauna ia cea mai bună mutare individuală.
% Dacă sunt mai multe table disponibile, ia tabla care este cea mai bună
% mutare individuală în raport cu U-board.
narrowGreedy([all|Bs], (UPos, M)) :- getUBoard([all|Bs], Ub), getNextPlayer([all|Bs], P), bestIndividualMoves(P, Ub, [UPos|_]), getBoard([all|Bs], UPos, B), bestIndividualMoves(P, B, [M|_]).
narrowGreedy([UPos|Bs], M) :- UPos \= all, getBoard([UPos|Bs], UPos, B), getNextPlayer([UPos|Bs], P), bestIndividualMoves(P, B, [M|_]).

% bestMoves/2
% bestMoves(+State, -Moves)
% Leagă Moves la o listă care conține toate mutările disponibile, în
% ordinea priorității lor, după ordonarea prezentată în enunț.
bestMoves([all|Bs], Ms) :- positions(Pos), getNextPlayer([all|Bs], P), getUBoard([all|Bs], Ub), bestIndividualMoves(P, Ub, [UPos|_]), constructBestMoves([all|Bs], UPos, Pos, Ums), sortMoves(Ums, Lsms), sortMoves(Lsms, Ms).
bestMoves([UPos|Bs], Ms) :- UPos \= all, positions(Pos), constructBestMoves([UPos|Bs], UPos, Pos, Ums), sortMoves(Ums, Lsms), sortMoves(Lsms, Ms).

% constructBestMoves(+State, +UPos, +Positions, -UnsortedMoves)
% Construieste lista de mutari, nesortate
constructBestMoves(_, _, [], []).
constructBestMoves(S, UPos, [H|T], Ums) :- getBoard(S, UPos, B), getPos(B, H, ''), getNextPlayer(S, P), movePriority(P, B, H, Lpn), wholeGamePriority(S, (UPos, H), Gpn), constructBestMoves(S, UPos, T, Tums), Ums = [(Lpn, (Gpn, H))|Tums].
constructBestMoves(S, UPos, [H|T], Ums) :- getBoard(S, UPos, B), \+getPos(B, H, ''), constructBestMoves(S, UPos, T, Ums).

% wholeGamePriority(+State, +Move, -Priority)
% Functie care calculeaza prioritatea unei mutari, la nivelul intregului joc.
wholeGamePriority(S, (UPos, Pos), 0) :- S = [all|_], makeMove(S, (UPos, Pos), Ns), getNextPlayer(S, P), getBoards(Ns, Bs), whoWon(Bs, P).
wholeGamePriority(S, (_, Pos), 0) :- S \= [all|_], makeMove(S, Pos, Ns), getNextPlayer(S, P), getBoards(Ns, Bs), whoWon(Bs, P).
wholeGamePriority(S, (UPos, Pos), 9) :- S = [all|_], makeMove(S, (UPos, Pos), Ns), getBoard(Ns, Pos, B), \+whoWon(B, '').
wholeGamePriority(S, (_, Pos), 9) :- S \= [all|_], makeMove(S, Pos, Ns), getBoard(Ns, Pos, B), \+whoWon(B, '').
wholeGamePriority(S, (UPos, Pos), 10) :- S = [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, (UPos, Pos), Ns), positions(Ps), getBoard(Ns, Pos, B), canWin(Opp, B, Ps), getUBoard(Ns, Ub), nth0(N, Ps, Pos, _), editList(Ub, Opp, N, 0, Nub), whoWon(Nub, Opp).
wholeGamePriority(S, (_, Pos), 10) :- S \= [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, Pos, Ns), positions(Ps), getBoard(Ns, Pos, B), canWin(Opp, B, Ps), getUBoard(Ns, Ub), nth0(N, Ps, Pos, _), editList(Ub, Opp, N, 0, Nub), whoWon(Nub, Opp).
wholeGamePriority(S, (UPos, Pos), 6) :- S = [all|_], getNextPlayer(S, P), makeMove(S, (UPos, Pos), Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(P, B, Ps).
wholeGamePriority(S, (_, Pos), 6) :- S \= [all|_], getNextPlayer(S, P), makeMove(S, Pos, Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(P, B, Ps).
wholeGamePriority(S, (UPos, Pos), 7) :- S = [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, (UPos, Pos), Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(Opp, B, Ps), winPos(Opp, B, Wp), makeMove(Ns, Wp, Ns2), getBoard(Ns2, Wp, B2), (\+whoWon(B2, ''); canWin(P, B2, Ps)).
wholeGamePriority(S, (_, Pos), 7) :- S \= [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, Pos, Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(Opp, B, Ps), winPos(Opp, B, Wp), makeMove(Ns, Wp, Ns2), getBoard(Ns2, Wp, B2), (\+whoWon(B2, ''); canWin(P, B2, Ps)).
wholeGamePriority(S, (UPos, Pos), 8) :- S = [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, (UPos, Pos), Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(Opp, B, Ps), winPos(Opp, B, Wp), makeMove(Ns, Wp, Ns2), getBoard(Ns2, Wp, B2), \+canWin(P, B2, Ps).
wholeGamePriority(S, (_, Pos), 8) :- S \= [all|_], getNextPlayer(S, P), nextPlayer(P, Opp), makeMove(S, Pos, Ns), getBoard(Ns, Pos, B), positions(Ps), canWin(Opp, B, Ps), winPos(Opp, B, Wp), makeMove(Ns, Wp, Ns2), getBoard(Ns2, Wp, B2), \+canWin(P, B2, Ps).
wholeGamePriority(S, (_, Pos), 1) :- getNextPlayer(S, P), nextPlayer(P, Opp), getBoard(S, Pos, B), count(B, Opp, 0).
wholeGamePriority(S, (_, Pos), 2) :- getNextPlayer(S, P), nextPlayer(P, Opp), getBoard(S, Pos, B), count(B, Opp, 1).
wholeGamePriority(S, (_, Pos), 3) :- getNextPlayer(S, P), nextPlayer(P, Opp), getBoard(S, Pos, B), count(B, Opp, OppC), count(B, P, PC), OppC >= 2, PC > OppC.
wholeGamePriority(S, (_, Pos), 4) :- getNextPlayer(S, P), nextPlayer(P, Opp), getBoard(S, Pos, B), count(B, Opp, OppC), count(B, P, PC), OppC >= 2, OppC >= PC.
wholeGamePriority(_, _, 5).

% winPos(+Player, +Board, -Position)
% Functie care gaseste pozitia pe care, daca jucatorul ar face mutarea, ar castiga tabla.
winPos(P, B, nw) :- getPos(B, nw, ''), movePriority(P, B, nw, 0).
winPos(P, B, n) :- getPos(B, n, ''), movePriority(P, B, n, 0).
winPos(P, B, ne) :- getPos(B, ne, ''), movePriority(P, B, ne, 0).
winPos(P, B, w) :- getPos(B, w, ''), movePriority(P, B, w, 0).
winPos(P, B, c) :- getPos(B, c, ''), movePriority(P, B, c, 0).
winPos(P, B, e) :- getPos(B, e, ''), movePriority(P, B, e, 0).
winPos(P, B, sw) :- getPos(B, sw, ''), movePriority(P, B, sw, 0).
winPos(P, B, s) :- getPos(B, s, ''), movePriority(P, B, s, 0).
winPos(P, B, se) :- getPos(B, se, ''), movePriority(P, B, se, 0).

% greedy/2
% greedy(+State, -Move)
% Strategie care alege cea mai bună mutare, bazat pe rezultatul lui
% bestMoves/2.
greedy([all|Bs], (UPos, Pos)) :- getUBoard([all|Bs], Ub), getNextPlayer([all|Bs], P), bestIndividualMoves(P, Ub, [UPos|_]), bestMoves([all|Bs], [Pos|_]).
greedy([UPos|Bs], Pos) :- UPos \= all, bestMoves([UPos|Bs], [Pos|_]).
