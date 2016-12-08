% Things to do
%   - Setup the token to be passed in the algorithm
%   - Function to send and receive tokens between nodes
%   - Function to check: A process never forwards the token through the same channel twice
%   - Function to check: A process only forwards the token to its parent when there is no other option
%   - Function for the Tarry's algorithm

-module(assign2).
-compile(export_all).

main() ->
    %File = "input.txt",
    List = get_all_lines([]),
    %io:format("~s~n", ["List:"]),
    %my_print(List),
    %List,

    % Get the initiator node
    Init = get_init(List),
    io:format("~s~n", ["Initiator:"]),
    io:format("~p~n", Init),

    % Get the graph
    Graph = get_graph(List),
    io:format("~s~n", ["Graph:"]),
    my_print(Graph),

    %List_of_nodes = get_nodes(Graph),
    %io:format("~s~n", ["Nodes:"]),
    %my_print(List_of_nodes),

    % Create processes and make a dictionary to associate nodes with Pid
    Map = create_processes(Graph),
    Dictionary = dict:from_list(Map),
    io:format("~s~n", ["Map:"]),
    my_print(Map),

    % Convert graph of nodes into a graph of Pids 
    Graph2 = convert_list_to_pid(Graph, Dictionary),
    io:format("~s~n", ["Graph2:"]),
    my_print(Graph2),

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
my_print([H|T]) ->      
    io:format("~p~n", [H]),
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
            %io:format("Node ~p got ~p from ~p~n", [self(), H, X]),
            %io:format("~p", [MPid]),
            MPid ! {self()},
            foo2(Node);

        _ ->
            %io:format("Node ~p got a bad message from ~p~n", [self(), X])
            io:format("foo error")
    end.

% Function to set up variables in child process
foo2(Node) ->
    receive
        {N, Init_flag} ->
            Name = [Node],
            Neighbours = N,
            Parent = [],

            % Not finished yet need to pass function to initiator and function to rest
            case Init_flag of
                true -> foo3(Name, Neighbours, self());
                false -> foo4(Name, Neighbours, Parent)
            end;
        _ ->
            ok
    after 5000
        -> io:format("foo2 error")
    end,

    receive
        {T} -> 
            io:format("Final List: "),
            my_print(T)
    end.

% Not complete
% Initiator function
foo3(Name, [Neighbour|Nbs], Parent) ->
    io:format("Initiator Name: ~p~n", Name),
    io:format("Token: ~p~n", [Name]),
    Neighbour ! {self(), Name},
    foo5(Name, Nbs, Parent).

foo5(Name, Neighbours, Parent) ->
    receive
        {Pid, Token} ->
            T = [hd(Name)|Token],
            io:format("Token: ~p~n", [T]),
            my_print(Neighbours),

            case Neighbours of
                [] -> Parent ! {lists:reverse(T)};
                [N|Nbs] -> 
                    N ! {self(), T},
                    %io:format("N = ~p~n", [N]),
                    foo5(Name, Nbs, Parent)
            end
    end.
    
% Not complete
% Non-Initiator function
foo4(Name, Neighbours, Parent) ->
    io:format("Name: ~p~n", Name),
    receive
        {Pid, Token} ->
            case Parent of
                [] ->
                    P = Pid,
                    %io:format("P =~p~n", [P]),
                    Valid_neighbours = lists:delete(P, Neighbours);
                    % my_print(Valid_neighbours);
                P ->
                    Valid_neighbours = Neighbours
            end,

            T = [hd(Name)|Token],
            io:format("Token: ~p~n", [T]),
            my_print(Valid_neighbours),

            case Valid_neighbours of
                [] -> P ! {self(), T};
                [N|Nbs] -> 
                    N ! {self(), T},
                    %io:format("N = ~p~n", [N]),
                    foo4(Name, Nbs, P)
            end
    end.

% Listen function for main process - send a list of Pids to child process of which they can communicate with
listen(Graph, Init) ->
    receive
        {Pid} ->
            %io:format("Received Key: ~c~n", Key),
            %my_print(Nodes),
            Neighbours = find_neighbours(Graph, Pid),
            Pid ! {Neighbours, Pid == Init},
            listen(Graph, Init);
        [X] ->
            io:format("Got a bad message ~p~n", X),
            io:format("listen error")
            %ok
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
