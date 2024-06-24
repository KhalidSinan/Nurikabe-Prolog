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



%Masa
%in every cell check if you are either in the last colomn or row,
%   if you are then dont check anything
%   else get the color of the cell and beside it, below it and
%   beside-below
%   if the four cells are blue then it's false
%   otherwise it's all true
no2by2sea(I,_):-
    grid_size(N,_),
    I==N,!.
no2by2sea(_,J):-
    grid_size(_,M),
    J == M,
    !.
no2by2sea(I, J):-
    I1 is I + 1,
    J1 is J + 1,
    solve_cell(I, J, Num),
    solve_cell(I1, J, Num2),
    solve_cell(I, J1, Num3),
    solve_cell(I1, J1, Num4),
%    format('Checking cells: (~w,~w,~w, ~w, ~w, ~w)~n', [I, J, Num,
%    Num2, Num3, Num4]),
    \+ (Num == blue, Num2 == blue, Num3 == blue, Num4 == blue),
    no2by2sea(I1,J),
    no2by2sea(I,J1)
    .

no_2_by_2_sea:-
    no2by2sea(1,1).



% get the adjacent cells for specific cell.
nearby_cells(I, J, [H1, H2, H3, H4]) :-
    solve_cell(I, J, Color), % get the cell color.
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    % if the adjacent cell have the same color as the specific cell,
    % add it to the list else add empty list.
    (solve_cell(I1, J, Color) -> H1 = (I1, J) ; H1 = []),
    (solve_cell(I2, J, Color) -> H2 = (I2, J) ; H2 = []),
    (solve_cell(I, J1, Color) -> H3 = (I, J1) ; H3 = []),
    (solve_cell(I, J2, Color) -> H4 = (I, J2) ; H4 = []).

remove_empty_lists([], []).
remove_empty_lists([H|T], [H|Result]) :-
    H \= [],
    remove_empty_lists(T, Result).
remove_empty_lists([[]|T], Result) :-
    remove_empty_lists(T, Result).

iterate_and_add([], _, AllCells, AllCells).
iterate_and_add([(I,J)|T], Visited, Acc, Result) :-
    % check if (I,J) exist in (Visited) list.
    (member((I,J), Visited) ->
        iterate_and_add(T, Visited, Acc, Result)
    ;
        add_element((I,J), Acc, NewAcc),
        all_nearby_cells(I, J, NewAcc, NewVisited, UpdatedAcc),
        iterate_and_add(T, NewVisited, UpdatedAcc, Result)
    ).


% return list of cells that form an island or a sea.
% 1- get the adjacent cells for the current cell. => ex: [(1,3),[],(2,3),[]]
% 2- remove the empty lists from the adjacent cells list (Cells) ex: [(1,3),(2,3)].
% 3- for each adjacent cell do the following steps:
%    - check if we have visited it previously using "member" on (Visited).
%    - if visited => skip the cell.
%    - if not visited => add the cell to the current result list (Acc)
%      and repeat the same algorithm for this cell.
all_nearby_cells(I,J, Acc, NewVisited, UpdatedAcc) :-
    nearby_cells(I,J,Cells),
    remove_empty_lists(Cells,FilteredCells),

    iterate_and_add(FilteredCells, [ (I, J) | Acc], Acc, UpdatedAcc),
    % update the visited list (current visited cell + all the visited cells = new visited list)
    NewVisited = [ (I, J) | Acc].

all_nearby_cells(I, J, AllCells) :-
    % if the passed cell was fixed cell of number 1,
    % then the result is the passed cell only.
    fxd_cell(I,J,1) -> AllCells = [(I,J)] ;
    all_nearby_cells(I, J, [], [(I, J)], AllCells).

add_element((X, Y), List, [(X,Y) | List]).


% Base Case
loop_to_count_fxd_cells_in_island([], 0).
% If Statement == true

loop_to_count_fxd_cells_in_island([(I,J)|T], CNT):-
    ( fxd_cell(I,J,_) ->
      loop_to_count_fxd_cells_in_island(T, CNT1),
      CNT is CNT1 + 1
    ;
        loop_to_count_fxd_cells_in_island(T, CNT)
    ).

% One Fixed Cell In Island
one_fixed_cell_in_island:-
    % Loop On Fixed Cells
    fxd_cell(I, J, _), % Now I Have I and J
    % Get All Cells For The Island
    all_nearby_cells(I,J, S),
    % Loop And Count Fixed Cells in Island
    loop_to_count_fxd_cells_in_island(S,CNT),
    % Check If CNT > 1, then false
    CNT > 1,!,
    % To Backtrack
    fail.

one_fixed_cell_in_island:- true.


% hamza - start

% island_number_equals_size :

% in this function i will get all the cells which contain number through (findall) and save cells in list then send the list to other function

find_cells_with_numbers() :-
    findall((X,Y,Value),fxd_cell(X,Y,Value), List),
     walk_on_cells_with_number(List).


% in this function i will walk on every cells in list and send cell to other function
% " in short this func its work like a for loop  "

walk_on_cells_with_number([]).
walk_on_cells_with_number([(X,Y,Value)|Tail]) :-
 count_of_nearby_cells(X,Y,Value),
    walk_on_cells_with_number(Tail).

% in this function i will call func(all_nearby_cells) which return the list is have nearby of cell  then i  will calculate length of this list then test condition " if the length of list equal the number in cell "

count_of_nearby_cells(X,Y,Value) :-
    all_nearby_cells(X,Y,Result), length(Result, Length) , Length =:= Value .

% hamza-end

% tima-start
%  A function that collects the element o an array
sum_list_of_value([], 0).
sum_list_of_value([H|T],Sum):-
    sum_list_of_value(T,Sum1),
    Sum is H+Sum1.

% A function that collect the number of sea cells
calculate_number_of_cells_sea(Sum):-
    solve_cell(I,J,blue),
    all_nearby_cells(I,J,List),
    length(List, Sum),!.
% A function bring A sum of the fxd_ cell value List
get_all_fxd_cells(Sum):-
    findall(Value,fxd_cell(_,_,Value),ListOfValue),
    sum_list_of_value(ListOfValue,Sum).
% A function bring the number of solved cell
solved_cell_count(Count) :-
    findall(_, solve_cell(_,_,_), Cells),
    length(Cells, Count).
% A function bring the first connected sea it finds then counts the
% number of its cell , then calculate the number of island cells
% then checks the total of grid if they are equal to the grid number it
% return true else false
one_sea:-
    calculate_number_of_cells_sea(Sum1),
    get_all_fxd_cells(Sum2),
    solved_cell_count(Sum3),
    Sum1+Sum2-1 =:= Sum3.
% tima_end

% Part two

grid_size(7,7).
print_grid():- \+get_row().
get_row():- grid_size(N,_) , between(1,N,X) ,\+get_col(X),nl,nl,fail.
get_col(X):- grid_size(_,M) , between(1,M,Y) ,( fxd_cell(X,Y,C) -> fxd_cell(X,Y,C) ; solve_cell(X,Y,C)) , write(C),write(' ') , fail.


% Add Sea if not added
assert_sea(I,J):-
    % Check if solve_cell exists dont add
    % if doesnt exist add cell
    % or return true to continue the next statements
    (\+ solve_cell(I,J,blue) -> asserta(solve_cell(I,J,blue)) ; true).

% Put Sea Around Ones
put_sea_around_ones :-
    % Get Fixed Cell that has value of 1
    fxd_cell(I, J, 1),
    % get the nearby cells
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    % add sea in nearby cells
    assert_sea(I,J1),
    assert_sea(I1,J),
    assert_sea(I,J2),
    assert_sea(I2,J),
    % to backtrack
    fail.
put_sea_around_ones:- true.

% To remove cells that are out of board
remove_cells_out_of_board:-
    retractall(solve_cell(_, 8, _)),
    retractall(solve_cell(_, 0, _)),
    retractall(solve_cell(8, _, _)),
    retractall(solve_cell(0, _, _)).

% print sea ( I , J )
get_sea:- solve_cell(I,J,blue), write(I), write(' '), write(J), nl, fail.
get_sea:- true.

% get Fixed cells in a list
get_fxd_cells(List):-
    findall((I,J), fxd_cell(I,J,_), List).

% Put Sea Between Cells Seperated By One
put_sea_between_cells_seperated_by_one :-
    % Get Random Fixed Cell
    fxd_cell(I, J, _),
    % get the nearby cells
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    % get the cells seperated by one
    I3 is I - 2,
    I4 is I + 2,
    J3 is J - 2,
    J4 is J + 2,
    % check if it is fixed then add the cell inbetween
    (   fxd_cell(I, J3 ,_) -> assert_sea(I,J1) ; true),
    (   fxd_cell(I3, J ,_) -> assert_sea(I1,J) ; true),
    (   fxd_cell(I, J4, _) -> assert_sea(I,J2) ; true),
    (   fxd_cell(I4, J, _) -> assert_sea(I2,J) ; true),
    % to backtrack
    fail.
put_sea_between_cells_seperated_by_one:-   true.


nearby_neighbors_cells(I, J, [H1, H2, H3, H4]) :-
    solve_cell(I, J, Color), % get the cell color.
    I1 is I - 1,
    J1 is J - 1,
    I2 is I - 1,
    J2 is J + 1,
    I3 is I +1,
    J3 is J-1,
    I4 is I +1,
    J4 is J+1,
    % if the adjacent cell have the same color as the specific cell,
    % add it to the list else add empty list.
    (solve_cell(I1, J1, Color) -> H1 = (I1, J1) ; H1 = []),
    (solve_cell(I2, J2, Color) -> H2 = (I2, J2) ; H2 = []),
    (solve_cell(I3, J3, Color) -> H3 = (I3, J3) ; H3 = []),
    (solve_cell(I4, J4, Color) -> H4 = (I4, J4) ; H4 = []).
