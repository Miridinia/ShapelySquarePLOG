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
    restrict_star(Board, NumRows, NumCols, Solution),
    restrict_square(Board, NumRows, NumCols, Solution),
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

search_list_coordinates([], _, []).
search_list_coordinates([[Row, Col]|Coordinates], Board, [Val|Vals]):-
    search_coordinates(Row, Col, Board, Val),
    search_list_coordinates(Coordinates, Board, Vals).

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

all_neighbour_coordinates(Row, Col, NumRows, NumCols, Neighbours):-
    RowUp is Row - 1,
    RowDown is Row + 1,
    ColLeft is Col - 1,
    ColRight is Col + 1,
    PossibleNeighbours = [[RowUp, Col], [RowDown, Col], [Row, ColLeft], [Row, ColRight]],
    remove_invalid(PossibleNeighbours, NumRows, NumCols, Neighbours).

remove_invalid([], _, _, []).
remove_invalid([H|Coordinates], NumRows, NumCols, [H|ValidCoordinates]):-
    H = [Row,Col],
    valid_coordinates(Row, Col, NumRows, NumCols),
    remove_invalid(Coordinates, NumRows, NumCols, ValidCoordinates).

remove_invalid([[Row,Col]|Coordinates], NumRows, NumCols, ValidCoordinates):-
    \+ valid_coordinates(Row, Col, NumRows, NumCols),
    remove_invalid(Coordinates, NumRows, NumCols, ValidCoordinates).

valid_coordinates(Row, Col, NumRows, NumCols):-
    Row =< NumRows,
    Col =< NumCols,
    Row > 0,
    Col > 0.

restrict_star(Board, NumRows, NumCols, Solution):-
    restrict_star_aux(Board, NumRows, NumCols, 1, Solution).

restrict_star_aux(Board, NumRows, NumCols, Row, Solution):-
    restrict_star_row(Board, Row, 1, NumRows, NumCols, Solution),
    NewRow is Row + 1,
    restrict_star_aux(Board, NumRows, NumCols, NewRow, Solution).

restrict_star_aux(_, NumRows, _, Row, _):-
    Row > NumRows.    

restrict_star_row(Board, Row, Col, NumRows, NumCols, Solution):-
    search_coordinates(Row, Col, Board, star),
    search_coordinates(Row, Col, Solution, Value),
    Value #> 1,
    Value #\= 4,
    Value #\= 6,
    Value #\= 8,
    Value #\= 9,
    all_neighbour_coordinates(Row, Col, NumRows, NumCols, Neighbours),
    search_list_coordinates(Neighbours, Solution, Values),
    restrict_star_neighbours(Values),
    NewCol is Col + 1,
    restrict_star_row(Board, Row, NewCol, NumRows, NumCols, Solution).

restrict_star_row(Board, Row, Col, NumRows, NumCols, Solution):-
    search_coordinates(Row, Col, Board, NotStar),
    NotStar \= star, 
    NewCol is Col + 1,
    restrict_star_row(Board, Row, NewCol, NumRows, NumCols, Solution).

restrict_star_row(_, _, Col, _, NumCols, _):-
    Col > NumCols.

restrict_star_neighbours([]).
restrict_star_neighbours([Val|Values]):-
    Val #\= 1,
    Val #\= 2,
    Val #\= 3, 
    Val #\= 5, 
    Val #\= 7, 
    restrict_star_neighbours(Values).

restrict_square(Board, NumRows, NumCols, Solution):-
    restrict_square_aux(Board, NumRows, NumCols, 1, Solution).

restrict_square_aux(Board, NumRows, NumCols, Row, Solution):-
    restrict_square_row(Board, Row, 1, NumRows, NumCols, Solution),
    NewRow is Row + 1,
    restrict_square_aux(Board, NumRows, NumCols, NewRow, Solution).

restrict_square_aux(_, NumRows, _, Row, _):-
    Row > NumRows.    

restrict_square_row(Board, Row, Col, NumRows, NumCols, Solution):-
    search_coordinates(Row, Col, Board, square),
    search_coordinates(Row, Col, Solution, Value),
    Value rem 5 #= 0,
    all_neighbour_coordinates(Row, Col, NumRows, NumCols, Neighbours),
    search_list_coordinates(Neighbours, Solution, Values),
    search_list_coordinates(Neighbours, Board, Symbols),
    restrict_square_neighbours(Value, Values, Symbols),
    NewCol is Col + 1,
    restrict_square_row(Board, Row, NewCol, NumRows, NumCols, Solution).

restrict_square_row(Board, Row, Col, NumRows, NumCols, Solution):-
    search_coordinates(Row, Col, Board, NotSquare),
    NotSquare \= square, 
    NewCol is Col + 1,
    restrict_square_row(Board, Row, NewCol, NumRows, NumCols, Solution).

restrict_square_row(_, _, Col, _, NumCols, _):-
    Col > NumCols.

restrict_square_neighbours(_, [], []).
restrict_square_neighbours(SquareValue, [_|Values], [diamond|Symbols]):-
    restrict_square_neighbours(SquareValue, Values, Symbols).

restrict_square_neighbours(SquareValue, [Val|Values], [NotDia|Symbols]):-
    NotDia \= diamond, 
    SquareValue #\= Val,
    restrict_square_neighbours(SquareValue, Values, Symbols).
    


