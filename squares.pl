:- use_module(library(clpfd)),
use_module(library(lists)).

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
    print(Solution).


row_sums(Sums):- Sums = [23, 20, 26, 20, 26, 19, 22]. 

twod_same_size(Board, Copy):-
    length(Board, Lenght),
    length(Copy, Lenght),
    oned_same_size(Board, Copy).

oned_same_size([],[]).
oned_same_size([H|RemainingBoard], [H2|RemainingCopy]) :-
    length(H, Lenght),
    length(H2, Lenght),
    oned_same_size(RemainingBoard, RemainingCopy).    

solve(Board, Sums, Solution) :-
    twod_same_size(Board, Solution),
    restrict_domain(Solution),
    restrict_row_sums(Sums, Solution),
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
