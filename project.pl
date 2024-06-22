
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




<<<<<<< Updated upstream

sum_list_of_value([], 0).
sum_list_of_value([H|T],Sum):-
    sum_list_of_value(T,Sum1),
    Sum is H+Sum1.
=======
# hamza -start
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

# hamza-end



get_all_fxd_cells(Sum):-
    findall(Value,fxd_cell(_,_,Value),ListOfValue),
    sum_list_of_value(ListOfValue,Sum).


calculate_number_of_cells_sea(Sum):-
    solve_cell(I,J,blue),
    all_nearby_cells(I,J,List),
    length(List, Sum),!.
=======
>>>>>>> Stashed changes



