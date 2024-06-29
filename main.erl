-module(main).

-export([start/2, networkInitiate/2, taskMonitor/2, node/4,findSuccessor/4,near_node/3]).

-import(controller,[listen/2,hopCount/0]).

-import(ftables,[fingertable_send/2]).

-import(nodeoper,[inrange/4,nodePrevious/2,getDistance/4,closest/5]).


near_node(Key, FingerNodeIds, State) ->
    case lists:member(Key, FingerNodeIds) of
        true -> Key;
        _ -> closest(Key, FingerNodeIds, -1, 10000000, State)
    end.


node(Hash, M, ChordNodes, _NodeState) -> 
    FingerTable = lists:duplicate(M, randomNode(Hash, ChordNodes)),
    NodeStateUpdated = dict:from_list([{id, Hash}, {predecessor, nil}, {finger_table, FingerTable}, {next, 0}, {m, M}]),
    listen(NodeStateUpdated,M).



findSuccessor(Hash, State, I,  M) -> 
    case dict:find((Hash + I) rem round(math:pow(2, M)), State) of
        error ->
             findSuccessor(Hash, State, I + 1, M);
        _ -> (Hash + I) rem round(math:pow(2, M))
    end.



randomNode(Node_id, []) -> Node_id;
randomNode(_, Nodes) -> lists:nth(rand:uniform(length(Nodes)), Nodes).


addNode(ChordNodes, M, State, TotalNodes) ->
    RemainingHashes = lists:seq(0, TotalNodes - 1, 1) -- ChordNodes,
    Hash = lists:nth(rand:uniform(length(RemainingHashes)), RemainingHashes),
    Pid = spawn(main, node, [Hash, M, ChordNodes, dict:new()]),
    %adds node to chord
    [Hash, dict:store(Hash, Pid, State)].



findPID(Hash, State) -> 
    case dict:find(Hash, State) of
        error -> nil;
        _ -> dict:fetch(Hash, State)
    end.


sendMessage(_, [], _) ->
    ok;
sendMessage(Key, ChordNodes, State) ->
    [First | Rest] = ChordNodes,
    Pid = findPID(First, State),
    Pid ! {lookup, First, Key, 0, self()},
    sendMessage(Key, Rest, State).


nodeCreate(ChordNodes, _, _, 0, State) -> 
    [ChordNodes, State];
nodeCreate(ChordNodes, TotalNodes, M, NumNodes, State) ->
    [Hash, NewState] = addNode(ChordNodes,  M, State, TotalNodes),
    nodeCreate(lists:append(ChordNodes, [Hash]), TotalNodes, M, NumNodes - 1, NewState).



sendMessageToAll(_, 0, _, _) ->
    ok;
sendMessageToAll(ChordNodes, NumRequest, M, State) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(ChordNodes)), ChordNodes),
    sendMessage(Key, ChordNodes, State),
    sendMessageToAll(ChordNodes, NumRequest - 1, M, State).



killAllNodes([], _) ->
    ok;
killAllNodes(ChordNodes, State) -> 
    [First | Rest] = ChordNodes,
    findPID(First, State) ! {kill},
    killAllNodes(Rest, State).

get_m(NumNodes) ->
    round(math:ceil(math:log2(NumNodes))).

taskMonitor(0, HCount) ->
    mainprocess ! {totalhops, HCount}
;
%listens for task completion
taskMonitor(NumRequests, HCount) ->
    receive 
        {completed, _Pid, HCountForTask, _Key} ->
            taskMonitor(NumRequests - 1, HCount + HCountForTask)
    end.



sendMessageAndKill(ChordNodes, NumNodes, NumRequest, M, State) ->
    register(taskcompletionmonitor, spawn(main, taskMonitor, [NumNodes * NumRequest, 0])),

    sendMessageToAll(ChordNodes, NumRequest, M, State),

    TotalHops = hopCount(),
    
    io:format("Mean Hops = ~p  \n Total Hops = ~p  \n  Number of Nodes = ~p  \n  Number of Requests = ~p\n", [TotalHops/(NumNodes * NumRequest), TotalHops, NumNodes , NumRequest]),
    killAllNodes(ChordNodes, State).



networkInitiate(NumNodes, NumRequest) ->
    M = get_m(NumNodes),
    [ChordNodes, State] = nodeCreate([], round(math:pow(2, M)), M, NumNodes, dict:new()),
    
    fingertable_send(State,M),
    sendMessageAndKill(ChordNodes, NumNodes, NumRequest, M, State).


start(NumNodes, NumRequest) ->
    register(mainprocess, spawn(main, networkInitiate, [NumNodes, NumRequest])),
    ok.
