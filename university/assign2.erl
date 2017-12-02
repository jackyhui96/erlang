%%%-------------------------------------------------------------------
%%% @author jackyhui
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Tarry's algorithm
%%% Using an initiator and a list nodes which represent the fully connected graph
%%% Initiator passes a token which passed around the graph and is returned back to main where it is outputted 
%%% @end
%%% Created : 01. Nov 2017 15:41
%%%-------------------------------------------------------------------
-module(assign2).
-compile(export_all).

% Tarry's algorithm
% Reads from stdin a file which contains the initiator and a list nodes which represent the fully connected graph
% Initiator passes a token which passed around the graph and is returned back to main where it is outputted 
main() ->
    % Check if input was successful
    case input() of
        {[Init], Graph} ->
            % Create processes and make a dictionary to associate nodes with Pids
            Map = create_processes(Graph),
            Dict = dict:from_list(Map),

            % Convert graph of nodes into a graph of Pids 
            PidGraph = convert_list_to_pid(Graph, Dict),
            IsValid = validate_pidgraph(PidGraph), 

            % Check for any errors in conversion
            case IsValid of
                false -> io:format("Error: invalid graph~n");
                true ->
                    Value = dict:find(Init, Dict),
                    % Check to see if initiator returns a value from dictionary
                    case Value of
                        % Listen back for responses to check if nodes are created
                        {ok, InitPid} -> listen(PidGraph, InitPid);
                        error -> io:format("error, can't find in dictionary~n")
                    end
            end;
        error ->
            io:format("Error: invalid input~n")
    end.

% Listen function for main process - send a list of Pids to child process of which they can communicate with
listen([], _) -> io:format("Listen error: empty graph~n");
listen(_, []) -> io:format("Listen error: empty initiator~n");
listen(Graph, Init) ->
    receive
        {request_neighbours, Pid} ->
            Neighbours = find_neighbours(Graph, Pid),
            Pid ! {self(), Neighbours, Pid =:= Init},
            listen(Graph, Init);
        {result, Result} ->
            my_print(Result);
        _ ->
            io:format("listen error~n")
    after 5000 ->
        timeout
    end.

% Function passed to each child process
start_node() ->
    receive
        {MainPid, Node} ->
            MainPid ! {request_neighbours, self()},
            setup_node(Node);
        _ ->
            ok
    end.
    
% Function to set up variables in child process, also listen for result from initiator
setup_node(Node) ->
    receive
        % {Neighbours, Initiator flag}
        {MainPid, Neighbours, true} ->
            init_start(Node, Neighbours, MainPid);
        {_, Neighbours, false} ->
            noninit_listen(Node, Neighbours, undefined)
    after 1000
        -> ok
    end.

% Initiator function - sends first message for Tarry's algorithm
init_start(Name, [N|Neighbours], Parent) ->
    N ! {self(), [Name]},
    init_listen(Name, Neighbours, Parent).
    
% Initiator function - listens for messages
init_listen(Name, Neighbours, MainPid) ->
    receive
        {_, Token} ->
            NewToken = [Name|Token],
            case Neighbours of
                % Reached end of algorithm, send back to main, reverse the token for right order
                [] -> MainPid ! {result, lists:reverse(NewToken)};
                % Send message to first neighbour and remove from the list of neighbours
                [N|Nbs] -> 
                    N ! {self(), NewToken},
                    init_listen(Name, Nbs, MainPid)
            end
    end.
        
% Non-Initiator function - listens for messages
noninit_listen(Name, Neighbours, Parent) ->
    receive
        {Pid, Token} ->
            NewToken = [Name|Token],

            % if no parent, assign parent and remove from list of neighbours
            {NewParent, NewNeighbours} = case Parent of
                undefined ->
                    {Pid, lists:delete(Pid, Neighbours)};
                _ ->
                    {Parent, Neighbours}
            end,

            case NewNeighbours of
                % No neighbours so send to parent
                [] -> NewParent ! {self(), NewToken};
                % Send message to first neighbour and remove from the list of neighbours
                [N|Nbs] -> 
                    N ! {self(), NewToken},
                    noninit_listen(Name, Nbs, NewParent)
            end
    end.

%%%===================================================================
%%% INPUT
%%%===================================================================
input() ->
    {
        ["p"],
        [
            ["p", "q", "s"],
            ["q", "p", "s", "t", "r"],
            ["r", "q", "t"],
            ["s", "p", "q", "t", "u"],
            ["t", "q", "r", "u", "s"],
            ["u", "s", "t"]
        ]
    }.

stdin_input() ->
    case get_all_lines([]) of
        [Init|Graph] ->
            {Init, Graph};
        _ ->
            error
    end.
    
% Reads input from stdin
get_all_lines(Accum) ->
    case io:get_line("") of
        eof  -> Accum;
        Line -> get_all_lines(Accum ++ [split_line(Line)])
    end.

% Splits the input by spaces and newlines - Helper function
split_line(Line) -> (string:tokens(Line, " $\n")).

%%%===================================================================
%%% OUTPUT
%%%===================================================================

% My function to print out the result
my_print(List) -> 
    Output = [ [L] || L <- List],
    io:format("~p~n", [Output]).

%%%===================================================================
%%% UTILITIES
%%%===================================================================

% Create processes from a list and create a list of tuples to associate nodes with pids 
create_processes(Graph) ->
    [ {Node, create_process(Node)} || [Node|_] <- Graph].
    
% Create a process, send the node name to that Pid, and return it's Pid - Helper function
create_process([]) -> ok;
create_process([Node|_]) ->
    Pid = spawn(assign2, start_node, []),
    Pid ! {self(), Node},
    Pid.

% Function to convert a list of nodes to a list of Pids
convert_list_to_pid(Graph, Dict) -> 
    [ lists:map(fun(X) -> 
                    convert_to_pid(X, Dict) 
                end, List) || List <- Graph ].

% Function to convert a key to its Pid - Helper function
convert_to_pid([], _) -> [];
convert_to_pid(Key, Dict) -> 
    case dict:find(Key, Dict) of
        {ok, Value} -> Value;
        error -> error
    end.

% Check that there is no error in the graph of Pids
validate_pidgraph(PidGraph) ->
    lists:all(fun(X) -> X =:= false end, [lists:member(error, List) || List <- PidGraph]).

% Find neighbours of the given Pid if it matches head of list
find_neighbours([], _) -> [];
find_neighbours([[Pid|Neighbours]|_], Pid) -> Neighbours;
find_neighbours([_|Rest], Pid) ->
    find_neighbours(Rest, Pid).
