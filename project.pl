
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
:- dynamic solve_cell/3.
solve_cell(1,1,'blue').
solve_cell(1,2,'green').
solve_cell(1,3,'green').
solve_cell(1,4,'green').
solve_cell(1,5,'blue').
solve_cell(1,6,'green').
solve_cell(1,7,'blue').


solve_cell(2,1,'blue').
solve_cell(2,2,'blue').
solve_cell(2,3,'blue').
solve_cell(2,4,'blue').
solve_cell(2,5,'blue').
solve_cell(2,6,'blue').
solve_cell(2,7,'blue').
%
solve_cell(3,1,'green').
solve_cell(3,2,'green').
solve_cell(3,3,'blue').
solve_cell(3,4,'green').
solve_cell(3,5,'blue').
solve_cell(3,6,'green').
solve_cell(3,7,'green').

%
solve_cell(4,1,'blue').
solve_cell(4,2,'blue').
solve_cell(4,3,'blue').
solve_cell(4,4,'blue').
solve_cell(4,5,'blue').
solve_cell(4,6,'blue').
solve_cell(4,7,'green').
%
solve_cell(5,1,'blue').
solve_cell(5,2,'green').
solve_cell(5,3,'blue').
solve_cell(5,4,'green').
solve_cell(5,5,'green').
solve_cell(5,6,'blue').
solve_cell(5,7,'green').
%
solve_cell(6,1,'blue').
solve_cell(6,2,'blue').
solve_cell(6,3,'green').
solve_cell(6,4,'blue').
solve_cell(6,5,'blue').
solve_cell(6,6,'blue').
solve_cell(6,7,'green').
%
solve_cell(7,1,'green').
solve_cell(7,2,'blue').
solve_cell(7,3,'green').
solve_cell(7,4,'blue').
solve_cell(7,5,'green').
solve_cell(7,6,'blue').
solve_cell(7,7,'green').

%Masa
%in every cell check if you are either in the last colomn or row,
%   if you are then dont check anything
%   else get the color of the cell and beside it, below it and
%   beside-below
%   if the four cells are blue then its false
%   otherwise its all true
no2by2sea_helper(I,_):-
    grid_size(N,_),
    I==N,!.
no2by2sea_helper(_,J):-
    grid_size(_,M),
    J == M,
    !.
no2by2sea_helper(I, J):-
    I1 is I + 1,
    J1 is J + 1,
    return_color(I, J, Num),
    return_color(I1, J, Num2),
    return_color(I, J1, Num3),
    return_color(I1, J1, Num4),
    \+ (Num == 'blue', Num2 == 'blue', Num3 == 'blue', Num4 == 'blue')
    .
no_2_by_2_sea:-
    grid_size(N,M),
    N1 is N - 1,
    M1 is M - 1,
    between(1,N1,I),
    between(1,M1,J),
    no2by2sea_helper(I,J).


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

nearby_empty_cells(I, J, Row, Col, [H1 , H2 , H3 , H4]) :-
I1 is I - 1,
I2 is I + 1,
J1 is J - 1,
J2 is J + 1,
% if the adjacent cell have the same color as the specific cell,
% add it to the list else add empty list.
(I1 > 0, I1 =< Row, \+solve_cell(I1, J, _) -> H1 = (I1, J) ; H1 = [] ),
(I2 > 0, I2 =< Row, \+solve_cell(I2, J, _) -> H2 = (I2, J) ; H2 = [] ),
(J1 > 0, J1 =< Col, \+solve_cell(I, J1, _) -> H3 = (I, J1) ; H3 = [] ),
(J2 > 0, J2 =< Col, \+solve_cell(I, J2, _) -> H4 = (I, J2) ; H4 = [] ).

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
    % if the cell not connected to any other add it and stop
    (nearby_cells(I, J, Cells),
     remove_empty_lists(Cells, FilteredCells),
      FilteredCells == [] ->
        AllCells = [(I, J)]
    ;
        all_nearby_cells(I, J, [], [(I, J)], AllCells)
    ).
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


island_number_equals_size :-
    find_cells_with_numbers().

find_cells_with_numbers() :-
    findall((X,Y,Value),fxd_cell(X,Y,Value), List),
     walk_on_cells_with_number(List).


walk_on_cells_with_number([]).
walk_on_cells_with_number([(X,Y,Value)|Tail]) :-
 count_of_nearby_cells(X,Y,Value),
    walk_on_cells_with_number(Tail).


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

% sea-expansion


% sea expansion algorithm:
% - loop through every sea cell
% - get nearby cells of current sea cell and filter them
% - if not empty skip this cell
%   because the sea cell is already connected to another sea cell
% - if empty get nearby empty cells and filter them
% - if length equal 1 make it sea
% - if not skip

expand_sea([(I1,J1) |_]):-
    assert_sea(I1,J1).


sea_expansion :-
    findall((I,J), solve_cell(I,J,blue),List),
    sea_expansion_helper(List).
sea_expansion:- true.

sea_expansion_helper([]).
sea_expansion_helper([(I,J) | T]) :-
    nearby_cells(I,J,Cells),
    remove_empty_lists(Cells,FilteredCells),
    (FilteredCells == [] -> (
    nearby_empty_cells(I,J,7,7,EmptyCells),
    remove_empty_lists(EmptyCells,FilteredEmptyCells),
    length(FilteredEmptyCells, N),
    (N == 1 ->
    expand_sea(FilteredEmptyCells)
    ; sea_expansion_helper(T))
    )
    ; sea_expansion_helper(T)).




% Add Sea if not added
assert_sea(I,J):-
    % Check if solve_cell exists dont add
    % if doesnt exist add cell
    % or return true to continue the next statements
    (\+ solve_cell(I,J,'blue') -> asserta(solve_cell(I,J,'blue')) ; true).

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
    % to backtrack,
       fail.

put_sea_around_ones:- true.

% To remove cells that are out of board
remove_cells_out_of_board:-
    grid_size(N,M),
    N1 is N + 1,
    M1 is M + 1,
    retractall(solve_cell(_, M1, _)),
    retractall(solve_cell(_, 0, _)),
    retractall(solve_cell(N1, _, _)),
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
put_sea_between_cells_seperated_by_one:- true.


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

% Diagonally adjacent clues - start (hamza) :

diagonally_adjacent_clues :- find_cells_with_number().

find_cells_with_number() :-
    findall((X,Y,Value), fxd_cell(X,Y,Value), List),
    walk_on_cells(List).

walk_on_cells([]).
walk_on_cells([(X,Y,_)|Tail]) :-
    mark_adjacent_cell(X, Y, 1, 1), % Check Right-Down
    mark_adjacent_cell(X, Y, -1, 1), % Check Left-Down
    mark_adjacent_cell(X, Y, -1, -1), % Check Left-Up
    mark_adjacent_cell(X, Y, 1, -1), % Check Right-Up
    walk_on_cells(Tail).

mark_adjacent_cell(X, Y, DX, DY) :-
    X1 is X + DX,
    Y1 is Y + DY,
    (fxd_cell(X1, Y1, _) ->
        (retractall(solve_cell(X, Y1, _)),
         asserta(solve_cell(X, Y1, 'blue')),
         retractall(solve_cell(X1, Y, _)),
        asserta(solve_cell(X1, Y, 'blue')))
    ; true). % Do nothing if the adjacent cell is not fixed .

% Diagonally adjacent clues - end (hamza) .


% Surrounded square - start (hamza) :

   is_empty_cell(X, Y) :-
   \+ solve_cell(X, Y,_),
    \+ fxd_cell(X, Y, _).

surrounded_square :-
    grid_size(GridX, GridY),  % Get the grid size
    X = 1,
    Y = 1,
    surrounded_square1(X, Y, GridX, GridY). % Pass grid size as arguments

surrounded_square1(X, _, GridX, _) :-
    X > GridX,  % Base case: Reached the end of the row
    !.

surrounded_square1(X, Y, GridX, GridY) :-
    processing(X, Y, GridX, GridY),
    X1 is X + 1,
    surrounded_square1(X1, Y, GridX, GridY).

processing(_, Y, _, GridY) :-
    Y > GridY, % Base case: Reached the end of the column
    !.

processing(X, Y, GridX, GridY) :-
        check(X, Y),
        Y1 is Y + 1,
        processing(X, Y1, GridX, GridY).

check(X, Y) :-
    (   is_empty_cell(X, Y),
       is_surrounded_by_sea(X, Y)
    )
    -> asserta(solve_cell(X, Y, 'blue')); true.

is_surrounded_by_sea(X, Y) :-
    grid_size(GridX, GridY),
    (   (X1 is X - 1, X1 >= 1, solve_cell(X1, Y, blue))
    ;   (X1 is X - 1, X1 < 1, true)
    ),
    (   (X2 is X + 1, X2 =< GridX, solve_cell(X2, Y, blue))
    ;   (X2 is X + 1, X2 > GridX, true)
    ),
    (   (Y1 is Y - 1, Y1 >= 1, solve_cell(X, Y1, blue))
    ;   (Y1 is Y - 1, Y1 < 1, true)
    ),
    (   (Y2 is Y + 1, Y2 =< GridY, solve_cell(X, Y2, blue))
    ;   (Y2 is Y + 1, Y2 > GridY, true)
    ).

 % Surrounded square - end (hamza) .


% Assert Land without duplicates
assert_land(I,J):-
    % Check if solve_cell exists dont add
    % if doesnt exist add cell
    % or return true to continue the next statements
    (\+ solve_cell(I,J,'green') -> asserta(solve_cell(I,J,'green')) ; true).

% get a cell that is green and isnt connected to any island
get_alone_cell(I,J):-
    solve_cell(I,J, 'green'),
    \+ fxd_cell(I,J,_),
    all_nearby_cells(I,J, List),
    length(List, Length),
    Length =:= 1.

% get fixed island value of this island
 get_fxd_island_value(I, J, Value, Length):-
    all_nearby_cells(I,J,List),
    member((X,Y), List),
    fxd_cell(X, Y, Value),
    length(List, Length).

% Island Continuity
island_continuity_helper(I,J):-
        get_fxd_island_value(I, J, Value, Length),
        Value > Length + 1.

island_continuity_diagonal([A,B,C,D], X, Y, X1, X2, Y1, Y2):-
    (  A \= [], A = (I,J),  island_continuity_helper(I, J) ->
            (   \+ solve_cell(X, Y2, _) ->  assert_land(X, Y2)
            ;   \+ solve_cell(X2, Y, _) -> assert_land(X2, Y) ;   true              );   true),
     (  B \= [],  B = (I,J), island_continuity_helper(I, J) ->
            (   \+ solve_cell(X, Y1, _) ->  assert_land(X, Y1)
            ;   \+ solve_cell(X2, Y, _) -> assert_land(X2, Y) ;   true              );   true),
     (  C \= [], C = (I,J), island_continuity_helper(I, J) ->
            (   \+ solve_cell(X, Y1, _) ->  assert_land(X, Y1)
            ;   \+ solve_cell(X1, Y, _) -> assert_land(X1, Y) ;   true              );   true),

     (  D \= [], D = (I,J), island_continuity_helper(I, J) ->
            (   \+ solve_cell(X, Y2, _) ->  assert_land(X, Y2)
            ;   \+ solve_cell(X1, Y, _) -> assert_land(X1, Y) ;   true              );   true).

island_continuity :-
    get_alone_cell(I,J),
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    % get the cells seperated by one
    I3 is I - 2,
    I4 is I + 2,
    J3 is J - 2,
    J4 is J + 2,
    nearby_neighbors_cells(I,J, S),
    island_continuity_diagonal(S, I, J, I1, I2, J1, J2),
    (   island_continuity_helper(I, J3) -> assert_land(I,J1) ; true),
    (   island_continuity_helper(I3, J) -> assert_land(I1,J) ; true),
    (   island_continuity_helper(I, J4) -> assert_land(I,J2) ; true),
    (   island_continuity_helper(I4, J) -> assert_land(I2,J) ; true).
island_continuity :- true.
%SARA
%new print method
grid_size(7,7).
print_grid:- \+get_row.
get_row:- grid_size(N,_) , between(1,N,X),\+get_col(X),nl,nl,fail.
get_col(X):- grid_size(_,M) , between(1,M,Y),
    (fxd_cell(X,Y,C) -> write('  '), write(C),write('    ');
    solve_cell(X,Y,green) -> write('green  ');
    solve_cell(X,Y,blue) -> write('blue   ');
    \+solve_cell(X,Y,_), write('  _    ')),
    fail.


%stop when no more cells
restart:- \+ solve_cell(_, _, _).
% check if there is a fact exist, retract the fact, recursive call to
% process the next cell
restart:-
    solve_cell(X, Y, Color),
    retract(solve_cell(X, Y, Color)),
    restart.


% when a green cell have only two directions to expand, then the
% diagonal cell will be sea
expandable_only_in_two_directions(I,J):-
   I1 is I - 1,
   I2 is I + 1,
   J1 is J - 1,
   J2 is J + 1,
   %check if the cell is an island
   solve_cell(I,J,green),
    %two cases: if the cell is fixed with 2 value, use "diagonal_cell_is_sea" directly, if its not
   %make sure that the island of this cell needs one more cell to complete, then use "diagonal_cell_is_sea"
  (fxd_cell(I,J,2)-> diagonal_cell_is_sea(I,J,I1,I2,J1,J2) ; all_nearby_cells(I,J,Result),
   member((X,Y),Result),
   fxd_cell(X,Y,Value),
   length(Result, Length),
   Length =:= Value - 1 -> diagonal_cell_is_sea(I,J,I1,I2,J1,J2)).
%this function check if there is two neighbor adjacents filled and the other two adjacents are empty,
%then add a sea in the diagonal cell from the empty cells direction
diagonal_cell_is_sea(I,J,I1,I2,J1,J2):-
    (\+solve_cell(I1,J,_),\+solve_cell(I,J1,_),solve_cell(I,J2,_),solve_cell(I2,J,_)-> assert_sea(I1,J1);
   \+solve_cell(I1,J,_),\+solve_cell(I,J2,_),solve_cell(I,J1,_),solve_cell(I2,J,_)-> assert_sea(I1,J2);
   \+solve_cell(I,J2,_),\+solve_cell(I2,J,_),solve_cell(I1,J,_),solve_cell(I,J1,_)-> assert_sea(I2,J2);
   \+solve_cell(I2,J,_),\+solve_cell(I,J1,_),solve_cell(I,J2,_),solve_cell(I1,J,_)-> assert_sea(I2,J1)).

%filling the empty adjacents with sea
fill_adjacents(I,J,Row,Col) :-
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    (I1 > 0, I1 =< Row, \+solve_cell(I1, J, _) ->  assert_sea(I1,J) ; true),
    (I2 > 0, I2 =< Row, \+solve_cell(I2, J, _) ->  assert_sea(I2,J) ; true),
    (J1 > 0, J1 =< Col, \+solve_cell(I, J1, _) ->  assert_sea(I,J1) ; true),
    (J2 > 0, J2 =< Col, \+solve_cell(I, J2, _) ->  assert_sea(I,J2) ; true).

% this function checks if an island is completed by comparing the number
% of its cells with the value of the fixed cell, if its completed then
% give the island cells list to "fill_adjacents_helper" function
surrounding_a_complete_island(FixedCells) :-
    member((I,J,Value),FixedCells),
    all_nearby_cells(I,J,AllCells),
    count_of_nearby_cells(I,J,Value),
    fill_adjacents_helper(AllCells),
    fail.
%loop for "fill_adjacents" function
fill_adjacents_helper([(I,J) | T]):-
    fill_adjacents(I,J,7,7),
    fill_adjacents_helper(T).
%tha main function, get all fixed cells and give them to "surrounding_a_complete_island" function

sea_around_island :-
    findall((I,J,Value),fxd_cell(I,J,Value),FixedCells),
    surrounding_a_complete_island(FixedCells).
sea_around_island.

% startegy 4 is : if a cell was surrounded by three sea , the fourth
% will be land
return_color(I,J,E):-
   grid_size(N,M),
   (I > N ; J > M ; I < 1; J < 1),
   E = 'blue',
   !.
return_color(I,J,E):-
    fxd_cell(I,J,_),
    E = 'blue',
    !.
return_color(I, J, E) :-
    (   solve_cell(I, J, X),
        X \= false -> E = X ;   E = 'a'
    ).

cnt_blue('blue', 1) :- !.
cnt_blue(_, 0).

begin_strategy_4(I, J) :-
    I1 is I + 1,
    I2 is I - 1,
    J1 is J + 1,
    J2 is J - 1,
    return_color(I1, J, C1),cnt_blue(C1, R1),
    return_color(I2, J, C2),cnt_blue(C2, R2),
    return_color(I, J1, C3),cnt_blue(C3, R3),
    return_color(I, J2, C4),cnt_blue(C4, R4),
    Total is R1 + R2 + R3 + R4,

    Total == 3,
    (
      ( C1 \= 'blue',C1 \= 'green',  assert_land(I1, J));
      ( C2 \= 'blue',C1 \= 'green',  assert_land(I2, J));
      ( C3 \= 'blue',C1 \= 'green',  assert_land(I, J1));
      ( C4 \= 'blue',C1 \= 'green',  assert_land(I, J2))
     ),
    fail.
begin_strategy_4(_,_).

do1:-
    fxd_cell(I, J, _),
    begin_strategy_4(I, J).
do2:-
    solve_cell(N,M,'green'),
    begin_strategy_4(N,M),
    fail.

island_expansion_from_a_clue :-
    do1,
    do2.
island_expansion_from_a_clue:- true.

%avoiding_wall_area_of_2by2 (Masa)
check_four_around_one(I, J) :-
    I1 is I + 1,
    J1 is J + 1,
    return_color(I, J, C1),cnt_blue(C1, R1),
    return_color(I1, J, C2),cnt_blue(C2, R2),
    return_color(I, J1, C3),cnt_blue(C3, R3),
    return_color(I1, J1, C4),cnt_blue(C4, R4),
    Total is R1 + R2 + R3 + R4,
    Total == 3,
    (
      ( C1 \= 'blue',C1 \= 'green', assert_land(I, J));
      ( C2 \= 'blue',C2 \= 'green', assert_land(I1, J));
      ( C3 \= 'blue',C3 \= 'green', assert_land(I, J1));
      ( C4 \= 'blue',C4 \= 'green', assert_land(I1, J1))
     ),
    fail.

avoiding_wall_area_of_2by2:-
    grid_size(N,M),
    N1 is N - 1,
    M1 is M - 1,
    between(1,N1,I),
    between(1,M1,J),
    check_four_around_one(I,J),
    fail.
avoiding_wall_area_of_2by2:- true.

% Wall Continuity (Tima)


return_cell_from_nearby_can_become_sea(List, (X,Y)) :-
    member((X, Y), List),
    \+solve_cell(X, Y, _),! .

return_cell_from_nearby_can_become_sea(_, (-1, -1)).

nearby_of_list_of_neighBors_helper(I, J, [H1, H2, H3, H4]) :-
    solve_cell(I , J, _), % get the cell color.
    grid_size(N,M),
    I1 is I - 1,
    I2 is I + 1,
    J1 is J - 1,
    J2 is J + 1,
    % if the adjacent cell have the same color as the specific cell,
    % add it to the list else add empty list.
    (I1>0,\+solve_cell(I1, J, _)-> H1 = (I1, J) ; H1 = []),
    (I2=<N,\+solve_cell(I2, J, _)-> H2 = (I2, J) ; H2 = []),
    (J1>0,\+solve_cell(I, J1, _) -> H3 = (I, J1) ; H3 = []),
    (J2=<M,\+solve_cell(I, J2, _) -> H4 = (I, J2) ; H4 = []).

nearby_of_list_of_neighBors([], _).
nearby_of_list_of_neighBors([(I,J)|T], S):-
    grid_size(N,M),
    nearby_of_list_of_neighBors_helper(I,J,L),
    remove_empty_lists(L,List),
    nearby_empty_cells(I,J,N,M,L2),
    intersect(List,L2,L3),
    return_cell_from_nearby_can_become_sea(L3,S),
    (S \= (-1, -1), ! ; nearby_of_list_of_neighBors(T,S)).

intersect([], _, []).
intersect([H|T], L, IntersectionList) :-
    member(H, L),
    !,
    intersect(T, L, RestIntersectionList),
    IntersectionList = [H|RestIntersectionList].
intersect([_|T], L, IntersectionList) :-
    intersect(T, L, IntersectionList).


give_cell_should_become_sea([(I,J)|T], S):-
    nearby_neighbors_cells(I,J,L1),
    remove_empty_lists(L1,List2),
    nearby_of_list_of_neighBors(List2,S),
    (S \= (-1, -1), ! ; give_cell_should_become_sea(T,S)).
give_cell_should_become_sea(_, (-1, -1)).

wall_continuity_helper((I,J)):- assert_sea(I,J).

wall_continuity:-
        solve_cell(I,J,'blue'),
    all_nearby_cells(I,J,List),
    give_cell_should_become_sea(List,S),
    (S \= (-1, -1) ->
        wall_continuity_helper(S)),!.
wall_continuity:- print_grid, true.


solved :-
    one_sea,
    no_2_by_2_sea,
    one_fixed_cell_in_island,
    island_number_equals_size.


check_from_distance(I, J) :-
        forall(fxd_cell(I2, J2, Value),
        (
            I3 is I2 - I,
            J3 is J2 - J,
            abs(I3, I4),
            abs(J3, J4),
            D is I4 + J4,
            (D >= Value, ! ; false)
        )
    ),!.
check_from_distance(_, _) :- false.

unreachable_square_helper(I, J, N, M) :-
    (
    \+ solve_cell(I, J, _),
    check_from_distance(I, J)
) ->
    assert_sea(I, J)
;
I1 is I - 1,
J1 is J - 1,
(   I1 > 0, unreachable_square_helper(I1, J, N, M) ; true),
(   J1 > 0 , unreachable_square_helper(I, J1, N, M) ; true).


unreachable_square:-
    grid_size(N,M),
    unreachable_square_helper(N,M, N, M).


solve :-

    print('unreachable square'),
    nl,
    nl,
    unreachable_square,
    print_grid,

    sleep(3),


    print('put sea around ones'),
    nl,
    nl,
    put_sea_around_ones,
    print_grid,

    sleep(3),


    print('put sea between cells seperated by one'),
    nl,
    nl,
    put_sea_between_cells_seperated_by_one,
    print_grid,

    sleep(3),



    print('diagonally adjacent clues'),
    nl,
    nl,
    diagonally_adjacent_clues,
    print_grid,

    sleep(3),


    print('avoiding wall area of 2 by 2'),
    nl,
    nl,
    avoiding_wall_area_of_2by2,
    print_grid,

    sleep(3),

    print('surrounded square'),
    nl,
    nl,
    surrounded_square,
    print_grid,

    sleep(3),


    print('sea expansion'),
    nl,
    nl,
    sea_expansion,
    print_grid,

    sleep(3),


    % here two directions
    %print('expandable only in two directions'),
    %nl,
    %island_expansion_from_a_clue,
    %print_grid,

    %sleep(3),



    print('island expansion from a clue'),
    nl,
    nl,
    island_expansion_from_a_clue,
    print_grid,

    sleep(3),


    print('island continuity'),
    nl,
    nl,
    island_continuity,
    print_grid,

    print('sea around island'),
    nl,
    nl,
    sea_around_island,
    print_grid,

    sleep(3),


    print('wall continuity'),
    nl,
    nl,
    wall_continuity,
    print_grid.









