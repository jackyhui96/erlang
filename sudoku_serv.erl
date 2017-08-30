
%%%-------------------------------------------------------------------
%%% @author jackyhui
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Aug Year 13:03
%%%-------------------------------------------------------------------
-module(sudoku_serv).
-author("jackyhui").

-behaviour(gen_server).

%% API
%- export([start_link/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DIGITS, "123456789").
-define(ROWS, "ABCDEFGHI").
-define(COLS, ?DIGITS).

-record(state, {squares, units, peers, input}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

solve_my_puzzle(Input) ->
    gen_server:call(?SERVER, {solve_my_puzzle, Input}).

solve_my_puzzles(Input) ->
    gen_server:call(?SERVER, {solve_my_puzzles, Input}).

get_random_puzzle() ->
    gen_server:call(?SERVER, get_random_puzzle).

get_random_puzzles(N) ->
    gen_server:call(?SERVER, {get_random_puzzles, N}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{squares=squares(), units=units(), peers=peers()}}.

handle_call({solve_my_puzzle, Input}, _From, State) ->
    Grid = convert_to_grid(Input, State),
    Solution = solve(Grid, State),
    display_grid(final, Solution),
    {reply, ok, State};

handle_call({solve_my_puzzles, Input}, _From, State) ->
    Solution = solve_grids(Input, State),
    {reply, Solution, State};

handle_call(get_random_puzzle, _From, State) ->
    Grid = random_grid(State),
    {reply, Grid, State};

handle_call({get_random_puzzles, N}, _From, State) ->
    Grids = random_grids(N, State),
    {reply, Grids, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Gives all 81 squares in the format "A1" where A is the row, 1 is the col
squares() ->
    cross(?ROWS, ?COLS).

%% Returns a list of all units (Columns, Rows, Boxes)
unitlist() ->
    ColUnits = [cross(?ROWS, [C]) || C <- ?COLS],
    RowUnits = [cross([R], ?COLS) || R <- ?ROWS],
    BoxUnits = [cross(Rs, Cs) || Rs <- ["ABC", "DEF", "GHI"],
                                 Cs <- ["123", "456", "789"]],
    ColUnits ++ RowUnits ++ BoxUnits.

%% Returns a list showing which squares are in which units
units() ->
    Squares = squares(),
    UnitList = unitlist(),
    Units = [{S, [U || U <- UnitList, lists:member(S,U)]} || S <- Squares],
    dict:from_list(Units).

%% Cross product of 2 lists
cross(ListA, ListB) ->
    [[A|[B]] || A <- ListA, B <- ListB].

%% Returns a list of squares which are peers to a square
%% i.e. all the squares in a squares unit list, excluding itself
peers() ->
    Squares = squares(),
    Units = units(),
    F = fun(X) ->
            UnitsOfX = dict:fetch(X, Units),
            UnitSquares = lists:append(UnitsOfX),
            SortUnitsOfX = lists:usort(UnitSquares),
            SortUnitsOfX--[X] end,
    dict:from_list([{S, F(S)} || S <- Squares]).


%% Convert grid into a tuple list of {square, char} with 0 or "." for empties
convert_to_grid(Grid, #state{squares=Squares}) ->
    Chars = [C || C <- Grid, lists:member(C, lists:seq($0,$9)) orelse C =:= 46],
    true = length(Chars) =:= 81,
    lists:zip(Squares, Chars).

convert_to_grid(Grid) ->
    Squares = squares(),
    Chars = [C || C <- Grid, lists:member(C, lists:seq($0,$9)) orelse C =:= 46],
    true = length(Chars) =:= 81,
    lists:zip(Squares, Chars).

%% Solve a Grid
solve(error, _) ->
    error;
solve(Grid, State) ->
    try
        search(parse_grid(Grid, State), State)
    catch
        throw:Dict ->
            Dict
    end.

solve(Grid) ->
    State = #state{squares=squares(), units=units(), peers=peers()},
    solve(Grid, State).


%% Search for a solution
search(error, _) -> error;
search(Dict, State) ->
    %% If all squares have a single value return solution
    %% Else, try find square with minimum amount of potential values
    %% and search for solution by trying to assign each digit

    Keys = dict:fetch_keys(Dict),
    Values = [dict:fetch(K, Dict) || K <- Keys],
    %% Checks whether all values are length 1
    CheckList = [length(V) =:= 1 || V <- Values],

    case lists:member(false, CheckList) of
        false ->
            throw(Dict);
        true ->
             %% Find the squares that have the least amount of potential values
            {_, Square, Digits} = lists:min([{length(V), S, V} || {S, V} <- dict:to_list(Dict), length(V) > 1]),
            some([search(assign_digit(Dict,Square,Digit,State), State) || Digit <- Digits])
    end.

%% Return the element that did not return an error
some([]) -> error;
some([S|Seq]) ->
    case S of
        error ->
            some(Seq);
        _ -> S
    end.

%% Fill in initial values and eliminate potential values for each square
parse_grid(Grid, State=#state{squares=Squares}) ->
    Values = [{S, ?DIGITS} || S <- Squares],
    InitialDigits = [{Square, D} || {Square, D} <- Grid, D =/= $0, D =/= 46],
    Dict = assign_digits(dict:from_list(Values), InitialDigits, State),
    Dict.

parse_grid(Grid) ->
    State = #state{squares=squares(), units=units(), peers=peers()},
    parse_grid(Grid, State).

%% Assign a list of digits and return a new list of potential values
assign_digits(Dict, [], _) ->
    Dict;
assign_digits(Dict, [{Square, Digit}|Rest], State) ->
    NewDict = assign_digit(Dict, Square, Digit, State),
    case NewDict of
        error -> error;
        _ ->
            assign_digits(NewDict, Rest, State)
    end.

%% Assign a single digit to a square by eliminating all other digits
assign_digit(Dict, Square, Digit, State) ->
    DigitsToBeRemoved = ?DIGITS -- [Digit],
    NewDict = eliminate_digits(Dict, Square, DigitsToBeRemoved, State),
    NewDict.


%% Eliminate digits from a square and update potential values
eliminate_digits(error, _, _, _) ->
    error;
eliminate_digits(Dicts, _, [], _) ->
    Dicts;

eliminate_digits(Dict, Square, [Digit|Rest], State=#state{peers=Peers, units=Units}) ->
    CurrentDigits = dict:fetch(Square, Dict),
    case lists:member(Digit, CurrentDigits) of
        false -> eliminate_digits(Dict, Square, Rest, State);
        true ->
            NewDigits = CurrentDigits -- [Digit],
            NewDict = dict:store(Square, NewDigits, Dict),

            %% If no potential values left then error
            %% If only a single potential value then remove from peers the value
            %% Else, make sure the removed digit is still in peers
            NewDict2 = case NewDigits of
                [] -> error;
                [OnlyVal] ->
                    PeersOfSquare = dict:fetch(Square, Peers),
                    eliminate_peers(NewDict, PeersOfSquare, OnlyVal, State);
                _ ->
                    PeersOfSquare = dict:fetch(Square, Peers),
                    case check_peers_for_digit(PeersOfSquare, Digit, NewDict) of
                        true -> NewDict;
                        false -> error
                    end
            end,
            %% If no error, check if some units have only a single potential value
            case NewDict2 of
                error -> error;
                _ ->
                    UnitsOfSquare = dict:fetch(Square, Units),
                    NewDict3 = check_places_for_digit(UnitsOfSquare, NewDict2, Digit, State),
                    eliminate_digits(NewDict3, Square, Rest, State)
            end
    end.

%% Eliminate digit from peers
eliminate_peers(Dict, [], _, _) ->
    % io:format("eliminate_peers empty~n"),
    Dict;
eliminate_peers(Dict, [Peer|Rest], Digit, State) ->
    % io:format("eliminate_peers~n"),
    NewDict = eliminate_digits(Dict, Peer, [Digit], State),
    eliminate_peers(NewDict, Rest, Digit, State).

%% Check if some units have a single potential value
check_places_for_digit(_, error, _, _) ->
    error;
check_places_for_digit([], Dict, _, _) ->
    % io:format("check_places_for_digit empty~n"),
    Dict;

check_places_for_digit([Unit|Rest], Dict, Digit, State) ->
    %% If a digit has only one place, assign the digit to that square
    PlacesForDigit = [Square || Square <- Unit, lists:member(Digit, dict:fetch(Square, Dict))],
    NewDict = case PlacesForDigit of
        [] -> error;
        [OnlyPlace] ->
            assign_digit(Dict, OnlyPlace, Digit, State);
        _ -> Dict
    end,
    check_places_for_digit(Rest, NewDict, Digit, State).


%% Check if the digit is still in the peers potential values
check_peers_for_digit(Peers, Digit, Dict) ->
    F = fun(X) -> XValues = dict:fetch(X, Dict),
                  lists:member(Digit, XValues) end,
    CheckList = [F(P) || P <- Peers],
    lists:member(true, CheckList).

%% Display a grid
display_grid(error) ->
    io:format("No solution for that puzzle.~n");
display_grid(Grid) ->
    SortedGrid = lists:sort(dict:to_list(Grid)),
    io:format("~n------------------------------------+------------------------------------+------------------------------------~n"),
    [print_square(Square)|| Square <- SortedGrid],
    io:format("------------------------------------+------------------------------------+------------------------------------~n"),
    ok.
%% Displays with less padding
display_grid(final, error) ->
    io:format("No solution for that puzzle.~n");
display_grid(final, Grid) ->
    SortedGrid = lists:sort(dict:to_list(Grid)),
    io:format("~n-------------------------------------~n"),
    [print_square(final, Square)|| Square <- SortedGrid],
    io:format("-------------------------------------~n"),
    ok.
%% Prints N number of spaces
print_spaces(N) ->
    [io:format(" ") || _ <- lists:seq(1,N)],
    ok.
%% Prints a single square
print_square({[Row,Col], Digits}) ->
    S1 = (10 - length(Digits)),
    case {Row, Col} of
        {$C, $9} ->
            io:format("~p", [Digits]),
            print_spaces(S1),
            io:format("~n------------------------------------+------------------------------------+------------------------------------~n");
        {$F, $9} ->
            io:format("~p", [Digits]),
            print_spaces(S1),
            io:format("~n------------------------------------+------------------------------------+------------------------------------~n");
        {Row, $9} ->
            io:format("~p", [Digits]),
            print_spaces(S1),
            io:format("~n");
        % X -> io:format("~n~p",[X]);
        {Row, Col} when (Col rem 3) == 0 ->
            io:format("~p", [Digits]),
            print_spaces(S1),
            io:format("|");
        {Row, Col}->
            io:format("~p", [Digits]),
            print_spaces(S1)
    end.

%% Prints a single square with less padding
print_square(final, {[Row,Col], Values}) ->
        S = 1,
        case {Row, Col} of
            {$C, $9} ->
                io:format("~p", [Values]),
                print_spaces(S),
                io:format("~n------------+------------+-----------~n");
            {$F, $9} ->
                io:format("~p", [Values]),
                print_spaces(S),
                io:format("~n------------+------------+-----------~n");
            {Row, $9} ->
                io:format("~p", [Values]),
                print_spaces(S),
                io:format("~n");
            % X -> io:format("~n~p",[X]);
            {Row, Col} when (Col rem 3) == 0 ->
                io:format("~p", [Values]),
                print_spaces(S),
                io:format("|");
            {Row, Col}->
                io:format("~p", [Values]),
                print_spaces(S)
        end.

random_grids(N ,State) ->
    [random_grid(State) || _ <- lists:seq(1,N)].

random_grid(State=#state{squares=Squares}) ->
    Values = [{S, ?DIGITS} || S <- Squares],
    Dict = dict:from_list(Values),
    ShuffledKeys = dict:fetch_keys(Dict),
    Grid = random_assign_grid(Dict, ShuffledKeys, State),
    case Grid of
        error ->
            random_grid(State);
        _ ->
            random_grid_string(Grid)
    end.

random_grid_string(Grid) ->
    SortedGrid = lists:sort(dict:to_list(Grid)),
    F = fun({_, Digits}) -> case Digits of
            [Digit] -> Digit;
            _ -> $0
        end
    end,
    [F(Square) ||Square <- SortedGrid].

random_assign_grid(_Dict, [], _) -> error;
random_assign_grid(Dict, [Square|Rest], State) ->
    F = fun(_Key, V) -> length(V) =:= 1 end,

    FilledSquares = dict:filter(F, Dict),
    NumOfUniqueVals = length(lists:usort(dict:to_list(FilledSquares))),

    case {dict:size(FilledSquares), NumOfUniqueVals} of
        {81, _} ->
            error;
        {N, M} when N > 16, M > 7 ->
            Dict;
        _ ->
            Vals = dict:fetch(Square, Dict),
            Length = length(Vals),
            NewDict = case Length =:= 1 of
                true ->
                    Dict;
                false ->
                    % io:format("assign~n"),
                    N2 = rand:uniform(Length),
                    Digit = lists:nth(N2, Vals),
                    assign_digit(Dict, Square, Digit, State)
            end,
            case NewDict of
                error ->
                    error;
                _ ->
                    random_assign_grid(NewDict, Rest, State)
            end
    end.

% Testing script
script() ->
    L = "400000805030000000000700000020000060000080400000010000000603070500200000104000000",
    Grid = convert_to_grid(L),
    % ParsedGrid = parse_grid(Grid),
    % io:format("~p", [ParsedGrid]).
    Result = solve(Grid),
    display_grid(final, Result).

solve_grids(Grids, State) ->
    F = fun(X) -> case X of
                        error -> false;
                        _ -> true
                  end
        end,
    [ F(solve(convert_to_grid(G, State), State)) || G <- Grids ].

% solve_grids(debug, Grids) ->
%     [ solve(G) || G <- Grids ].

shuffle_list(List) ->
    Length = length(List),
    RandomList = [{rand:uniform(Length), Elem}  || Elem <- List],
    [Elem || {_, Elem} <- lists:sort(RandomList)].