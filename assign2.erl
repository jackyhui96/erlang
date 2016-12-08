-module(assign2).
-compile(export_all).

main() ->
    List = get_all_lines([]),

    % Get the initiator node
    Init = get_init(List),

    % Get the graph
    Graph = get_graph(List),

    % Create processes and make a dictionary to associate nodes with Pid
    Map = create_processes(Graph),
    Dictionary = dict:from_list(Map),

    % Convert graph of nodes into a graph of Pids 
    Graph2 = convert_list_to_pid(Graph, Dictionary),

    % Listen back for responses to check if nodes are created
    listen(Graph2, dict:fetch(hd(Init), Dictionary)).

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

% Get the initiator from a list (Head of list)
get_init([]) -> ok;
get_init([H|_]) -> H.

% Get the graph from a list (Tail of list)
get_graph([]) -> ok;
get_graph([_|T]) -> T.

% Get a list of nodes
get_nodes([]) -> [];
get_nodes([H|T]) -> [get_node(H)] ++ get_nodes(T).
% Get a single node
get_node([]) -> [];
get_node([H|_]) -> H. 

% Function to convert a list of nodes to a list of Pids
convert_list_to_pid([], _) -> [];
convert_list_to_pid([H|T], Dict) -> [[convert_to_pid(X, Dict) || X <- H]] ++ convert_list_to_pid(T, Dict).     

% Function to convert a key to its Pid
convert_to_pid([], _) -> [];
convert_to_pid(Key, Dict) -> dict:fetch(Key,Dict).

% Create processes from a list and create a list of tuples to associate nodes with pids 
create_processes([]) -> [];
create_processes([H|T]) ->
    Pid = create_process(H),
    case H of
        [] -> [];
        [X|_] -> [{X,Pid}] ++ create_processes(T)
    end.

% Create a process and return it's Pid
create_process([]) -> ok;
create_process([H|_]) ->
    Pid = spawn(assign2, foo, []),
    Pid ! {self(), H},
    Pid.

% Function passed to each child process (Need to change name)
foo() ->
    receive
        {MPid, Node} ->
            MPid ! {self()},
            foo2(Node);
        _ ->
            ok
    end.

% Function to set up variables in child process
foo2(Node) ->
    receive
        {N, Init_flag} ->
            Name = [Node],
            Neighbours = N,
            Parent = [],

            case Init_flag of
                true -> foo3(Name, Neighbours, self());
                false -> foo4(Name, Neighbours, Parent)
            end;
        _ ->
            ok
    after 2000
        -> ok
    end,

    receive
        {T} -> 
            io:format("["),
            my_print(T),
            io:format("]~n")
    end.

% Initiator function
foo3(Name, [Neighbour|Nbs], Parent) ->
    Neighbour ! {self(), Name},
    foo5(Name, Nbs, Parent).

foo5(Name, Neighbours, Parent) ->
    receive
        {Pid, Token} ->
            T = [hd(Name)|Token],

            case Neighbours of
                [] -> Parent ! {lists:reverse(T)};
                [N|Nbs] -> 
                    N ! {self(), T},
                    foo5(Name, Nbs, Parent)
            end
    end.
    
% Non-Initiator function
foo4(Name, Neighbours, Parent) ->
    receive
        {Pid, Token} ->
            case Parent of
                [] ->
                    P = Pid,
                    Valid_neighbours = lists:delete(P, Neighbours);
                P ->
                    Valid_neighbours = Neighbours
            end,

            T = [hd(Name)|Token],

            case Valid_neighbours of
                [] -> P ! {self(), T};
                [N|Nbs] -> 
                    N ! {self(), T},
                    foo4(Name, Nbs, P)
            end
    end.

% Listen function for main process - send a list of Pids to child process of which they can communicate with
listen(Graph, Init) ->
    receive
        {Pid} ->
            Neighbours = find_neighbours(Graph, Pid),
            Pid ! {Neighbours, Pid == Init},
            listen(Graph, Init);
        [X] ->
            io:format("listen error")
    after 2000
        -> ok
    end.

% Find neighbours of the given Pid if it matches head of list
find_neighbours([], _) -> [];
find_neighbours([H|T], Pid) ->
    case hd(H) == Pid of
        true -> tl(H);
        false -> find_neighbours(T, Pid)
    end.

