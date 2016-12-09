-module(assign2).
-compile(export_all).

% Tarry's algorithm
% Reads from stdin a file which contains the initiator and a list nodes which represent the fully connected graph
% Initiator passes a token which passed around the graph and is returned back to main where it is outputted 
main() ->
    % Get list from input
    List = get_all_lines([]),

    % Validate list - cannot be empty
    case List of
        [H|T] ->
            % Get the initiator node 
            Init = H,
            % Get the graph
            Graph = T,

            % Create processes and make a dictionary to associate nodes with Pids
            Map = create_processes(Graph),
            D = dict:from_list(Map),

            % Convert graph of nodes into a graph of Pids 
            Pid_graph = convert_list_to_pid(Graph, D),

            % Check for any errors in conversion
            case lists:member(error, Pid_graph) of
                true -> io:format("error in graph~n");
                false ->
                    Value = dict:find(hd(Init), D),
                    % Check to see if initiator returns a value from dictionary
                    case Value of
                        % Listen back for responses to check if nodes are created
                        {ok, Init_pid} -> listen(Pid_graph, Init_pid);
                        error -> io:format("error, can't find in dictionary~n")
                    end
            end;
        _ ->
            io:format("error in list~n")
    end.
            
% Reads input from stdin
get_all_lines(Accum) ->
    case io:get_line("") of
        eof  -> Accum;
        Line -> get_all_lines(Accum ++ [split_line(Line)])
    end.

% Splits the input by spaces and newlines - Helper function
split_line(Line) -> (string:tokens(Line, " $\n")).

% My function to print out lists
my_print([]) -> ok;
my_print([H|[]]) -> io:format("~p", [H]);
my_print([H|T]) ->      
    io:format("~p,", [H]),
    my_print(T).

% Function to convert a list of nodes to a list of Pids
convert_list_to_pid([], _) -> [];
convert_list_to_pid([H|T], Dict) -> [[convert_to_pid(X, Dict) || X <- H]] 
                                    ++ convert_list_to_pid(T, Dict).     
% Function to convert a key to its Pid - Helper function
convert_to_pid([], _) -> [];
convert_to_pid(Key, Dict) -> 
    % Using find avoids exception if key is not found
    case dict:find(Key,Dict) of
        {ok, Value} -> Value;
        error -> error
    end.

% Listen function for main process - send a list of Pids to child process of which they can communicate with
listen([], _) -> io:format("listen error, empty graph~n");
listen(_, []) -> io:format("listen error, empty initiator~n");
listen(Graph, Init) ->
    receive
        {Pid} ->
            Neighbours = find_neighbours(Graph, Pid),
            Pid ! {Neighbours, Pid == Init},
            listen(Graph, Init);
        _ ->
            io:format("listen error~n")
    after 1000
        -> ok
    end.

% Find neighbours of the given Pid if it matches head of list
find_neighbours([], _) -> [];
find_neighbours([H|T], Pid) ->
    case hd(H) == Pid of
        true -> tl(H);
        false -> find_neighbours(T, Pid)
    end.

% Create processes from a list and create a list of tuples to associate nodes with pids 
create_processes([]) -> [];
create_processes([H|T]) ->
    Pid = create_process(H),
    case H of
        [] -> [];
        [Node|_] -> [{Node,Pid}] ++ create_processes(T)
    end.
% Create a process and return it's Pid - Helper function
create_process([]) -> ok;
create_process([Node|_]) ->
    Pid = spawn(assign2, start_node(), []),
    Pid ! {self(), Node},
    Pid.

% Function passed to each child process
start_node() ->
    receive
        {Main_Pid, Node} ->
            Main_Pid ! {self()},
            setup_node(Node);
        _ ->
            ok
    end.

% Function to set up variables in child process, also listen for result from initiator
setup_node(Node) ->
    receive
        % {Neighbours, Initiator flag}
        {Nbs, Init_flag} ->
            case Init_flag of
                true -> init_start(Node, Nbs, self());
                false -> noninit_listen(Node, Nbs, [])
            end;
        _ ->
            ok
    after 1000
        -> ok
    end,

    % Listen for result from initiator and output
    receive
        {Token} -> 
            io:format("["),
            my_print(Token),
            io:format("]~n")
    after 1000
        -> ok
    end.

% Initiator function - sends first message for Tarry's algorithm
init_start(Name, [Neighbour|Nbs], Parent) ->
    Neighbour ! {self(), [Name]},
    init_listen(Name, Nbs, Parent).
% Initiator function - listens for messages
init_listen(Name, Neighbours, Main_pid) ->
    receive
        {_, Token} ->
            T = [Name|Token],

            case Neighbours of
                % Reached end of algorithm, send back to main, reverse the token for right order
                [] -> Main_pid ! {lists:reverse(T)};
                % Send message to first neighbour and remove from the list of neighbours
                [N|Nbs] -> 
                    N ! {self(), T},
                    init_listen(Name, Nbs, Main_pid)
            end
    end.
    
% Non-Initiator function - listens for messages
noninit_listen(Name, Neighbours, Parent) ->
    receive
        {Pid, Token} ->
            T = [Name|Token],

            % if no parent, assign parent and remove from list of neighbours
            case Parent of
                [] ->
                    P = Pid,
                    Valid_neighbours = lists:delete(P, Neighbours);
                P ->
                    Valid_neighbours = Neighbours
            end,

            case Valid_neighbours of
                % No neighbours so send to parent
                [] -> P ! {self(), T};
                % Send message to first neighbour and remove from the list of neighbours
                [N|Nbs] -> 
                    N ! {self(), T},
                    noninit_listen(Name, Nbs, P)
            end
    end.
