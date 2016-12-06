-module(assign2).
-compile(export_all).

main() ->
    File = "input.txt",
    List = readlines(File),
    % io:format("~s~n", ["List:"]),
    % my_print(List),
    % List,
    Init = get_init(List),
    io:format("~s~n", ["Initiator:"]),
    io:format("~p~n", Init),

    Graph = get_graph(List),
    io:format("~s~n", ["Graph:"]),
    my_print(Graph),

    %List_of_nodes = get_nodes(Graph),
    %io:format("~s~n", ["Nodes:"]),
    %my_print(List_of_nodes),

    Map = create_processes(Graph),
    Dictionary = dict:from_list(Map),
    io:format("~s~n", ["Map:"]),
    my_print(Map),
    listen().

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [split_line(Line)])
    end.

split_line(Line) -> (string:tokens(Line, " $\n")).

my_print([]) -> ok;
my_print([H|T]) ->  
    io:format("~p~n", [H]),
    my_print(T).

get_init([]) -> ok;
get_init([H|_]) -> H.

get_graph([]) -> ok;
get_graph([_|T]) -> T.

get_nodes([]) -> [];
get_nodes([H|T]) ->
    [get_node(H)] ++ get_nodes(T).

get_node([]) -> [];
get_node([H|_]) ->
    H. 

% Not finished
convert_list_to_pid([], Dict) -> [];
convert_list_to_pid([H|T], Dict) -> ok. 

convert_to_pid([], Dict) -> [];
convert_to_pid([H|T], Dict) -> dict:fetch(H,Dict).



create_processes([]) -> [];
create_processes([H|T]) ->
    Pid = create_process(H),
    case H of
        [] -> [];
        [X|_] -> [{X,Pid}] ++ create_processes(T)
    end.

create_process([]) -> ok;
create_process(Nodes) ->
    Pid = spawn(assign2, foo, [Nodes]),
    Pid ! {self()},
    Pid.

foo([H|T]) ->
    receive
        {X} ->
            io:format("Node ~p got ~p from ~p~n", [self(), H, X]),
            X ! [H, T];
        X ->
            io:format("Node ~p got a bad message from ~p~n", [self(), X])
    end.

listen() ->
    receive
        [Key|Nodes] ->
            io:format("Received Key: ~c~n", Key),
            my_print(Nodes),
            listen();
        _ ->
            io:format("Got a bad message~n")
    after 2000
        -> ok
    end.


