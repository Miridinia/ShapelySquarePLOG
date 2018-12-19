:- use_module(library(clpfd)).
:- use_module(library(lists)).

board(Board):- Board = [
    [square, knight, blank, diamond, square, circle, blank],
    [square, square, triangle, circle, square, square, blank],
    [circle, blank, triangle, diamond, knight, square, triangle],
    [heart, square, triangle, blank, diamond, blank, square],
    [heart, circle, diamond, circle, star, square, square],
    [square, triangle, square, square, square, square, knight],
    [circle, triangle, diamond, blank, circle, square, triangle]
].

test:-
    board(Board),
    row_sums(Sums),
    solve(Board, Sums, Solution),
    print_board(Solution).

print_board([]).
print_board([Row|Board]):-
    print(Row),
    nl,
    print_board(Board).

row_sums(Sums):- Sums = [23, 20, 26, 20, 26, 19, 22]. 

twod_same_size(Board, Copy, NumRows, NumCols):-
    length(Board, NumRows),
    length(Copy, NumRows),
    oned_same_size(Board, Copy, NumCols).

oned_same_size([],[], _).
oned_same_size([H|RemainingBoard], [H2|RemainingCopy], NumCols) :-
    length(H, NumCols),
    length(H2, NumCols),
    oned_same_size(RemainingBoard, RemainingCopy, NumCols).    

solve(Board, Sums, Solution) :-
    twod_same_size(Board, Solution, NumRows, NumCols),   
    restrict_domain(Solution),
    restrict_row_sums(Sums, Solution),
    restrict_circle(Board, Solution),
    restrict_diamond(Board, Solution),
    restrict_triangle(Board, NumRows, NumCols, Solution),
    flatten(Solution, FlatVariables),
    labeling([], FlatVariables).

restrict_domain([]).
restrict_domain([Row|Solution]):- 
    domain(Row, 0, 9),
    restrict_domain(Solution).

flatten([], []).
flatten([H|T], Flat):-
    flatten(T, FlatT),
    append(H, FlatT, Flat). 

restrict_row_sums([], []).
restrict_row_sums([RowSum|Sums],[Row|Solution]):-
    sum(Row, #=, RowSum),
    restrict_row_sums(Sums, Solution).

restrict_circle(Board, Solution):-
    get_all_circles(Board, Solution, [H|CircleVars]),
    not_multiple_three(H),
    all_equal([H|CircleVars]).

all_equal([_]).
all_equal([]).
all_equal([H1|[H2|T]]) :-
    H1 #= H2,
    all_equal([H2|T]).

not_multiple_three(Num):-
    Num rem 3 #\= 0.

get_all_circles([], [], []).
get_all_circles([Row|Board], [SolRow|Solution], CircleVars):-
    get_circles_row(Row, SolRow, CircleRow),
    get_all_circles(Board, Solution, OtherCircles),
    append(CircleRow, OtherCircles, CircleVars).

get_circles_row([], [], []).
get_circles_row([circle|Row], [CircleVar|SolRow], [CircleVar|CircleRow]):-
    get_circles_row(Row, SolRow, CircleRow).

get_circles_row([NotCircle|Row], [_|SolRow], CircleRow):-
    NotCircle \= circle,
    get_circles_row(Row, SolRow, CircleRow).


restrict_diamond([], []).
restrict_diamond([Row|Board], [RowSol|Solution]):-
    restrict_diamond_row(Row, RowSol),
    restrict_diamond(Board, Solution).

restrict_diamond_row(Row, RowSol):-
    restrict_diamond_row_aux(Row, RowSol, 0).

restrict_diamond_row_aux([], [], _).
restrict_diamond_row_aux([diamond|Row], [SolH|RowSol], LeftSum):-
    SolH rem 2 #\= 0,
    SolH #= LeftSum,
    SolH + LeftSum #= NewSum,
    restrict_diamond_row_aux(Row, RowSol, NewSum).

restrict_diamond_row_aux([NotDia|Row], [SolH|RowSol], LeftSum):-
    NotDia \= diamond,
    SolH + LeftSum #= NewSum,
    restrict_diamond_row_aux(Row, RowSol, NewSum).
    
search_coordinates(RowNum, ColNum, Board, Val):-
    nth1(RowNum, Board, Row),
    nth1(ColNum, Row, Val).

restrict_triangle(Board, NumRows, NumCols, Solution):-
    restrict_triangle_aux(Board, NumRows, NumCols, 1, Solution).

restrict_triangle_aux(Board, NumRows, NumCols, Row, Solution):-
    restrict_triangle_row(Board, Row, 1, NumCols, Solution),
    NewRow is Row + 1,
    restrict_triangle_aux(Board, NumRows, NumCols, NewRow, Solution).

restrict_triangle_aux(_, NumRows, _, Row, _):-
    Row > NumRows.

restrict_triangle_row(Board, Row, Col, NumCols, Solution):-
    search_coordinates(Row, Col, Board, triangle),
    UpRow is Row - 1,
    search_coordinates(UpRow, Col, Solution, UpValue),
    UpValue rem 2 #= 0,
    search_coordinates(Row, Col, Solution, Value),
    Value #< UpValue,
    Value #\= 0,
    NewCol is Col + 1,
    restrict_triangle_row(Board, Row, NewCol, NumCols, Solution).

restrict_triangle_row(Board, Row, Col, NumCols, Solution):-
    search_coordinates(Row, Col, Board, NotTriangle),
    NotTriangle \= triangle, 
    NewCol is Col + 1,
    restrict_triangle_row(Board, Row, NewCol, NumCols, Solution).

restrict_triangle_row(_, _, Col, NumCols, _):-
    Col > NumCols.

