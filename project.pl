
% fxd_cell(Row,col,num).
fxd_cell(1,2,3).
fxd_cell(1,6,1).

fxd_cell(3,1,2).
fxd_cell(3,4,1).

fxd_cell(5,2,1).
fxd_cell(5,5,2).

fxd_cell(6,3,2).

fxd_cell(7,1,1).
fxd_cell(7,5,1).
fxd_cell(7,7,6).



% solve_cell()
solve_cell(1,1,blue).
solve_cell(1,2,green).
solve_cell(1,3,green).
solve_cell(1,4,green).
solve_cell(1,5,blue).
solve_cell(1,6,green).
solve_cell(1,7,blue).


solve_cell(2,1,blue).
solve_cell(2,2,blue).
solve_cell(2,3,blue).
solve_cell(2,4,blue).
solve_cell(2,5,blue).
solve_cell(2,6,blue).
solve_cell(2,7,blue).

solve_cell(3,1,green).
solve_cell(3,2,green).
solve_cell(3,3,blue).
solve_cell(3,4,green).
solve_cell(3,5,blue).
solve_cell(3,6,green).
solve_cell(3,6,blue).
solve_cell(3,7,green).


solve_cell(4,1,blue).
solve_cell(4,2,blue).
solve_cell(4,3,blue).
solve_cell(4,4,blue).
solve_cell(4,5,blue).
solve_cell(4,6,blue).
solve_cell(4,7,green).

solve_cell(5,1,blue).
solve_cell(5,2,green).
solve_cell(5,3,blue).
solve_cell(5,4,green).
solve_cell(5,5,green).
solve_cell(5,6,blue).
solve_cell(5,7,green).

solve_cell(6,1,blue).
solve_cell(6,2,blue).
solve_cell(6,3,green).
solve_cell(6,4,blue).
solve_cell(6,5,blue).
solve_cell(6,6,blue).
solve_cell(6,7,green).

solve_cell(7,1,green).
solve_cell(7,2,blue).
solve_cell(7,3,green).
solve_cell(7,4,blue).
solve_cell(7,5,green).
solve_cell(7,6,blue).
solve_cell(7,7,green).


no_2_by_2_sea(_,7):- !.
no_2_by_2_sea(7,_):- !.



no_2_by_2_sea(_, 7) :- !.
no_2_by_2_sea(7, _) :- !.

no_2_by_2_sea(I, J) :-
    solve_cell(I, J, Num),
    I1 is I + 1,
    J1 is J + 1,
    solve_cell(I1, J, Num2),
    solve_cell(I, J1, Num3),
    solve_cell(I1, J1, Num4),
%    format('Checking cells: (~w, ~w, ~w, ~w, ~w)~n', [Num, Num2, Num3,
%    Num4, blue]),

    \+ (Num == blue, Num2 == blue, Num3 == blue, Num4 == blue),

    no_2_by_2_sea(I1, J),
    no_2_by_2_sea(I, J1).
























