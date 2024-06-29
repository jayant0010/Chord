-module(ftables).
-import(main,[findSuccessor/4]).
-export([tableData/5,fingertable_data/4,sendFingerTabletoNode/3,fingertable_send/2]).



fingertable_data(_, [], FTDict,_) ->
    FTDict;

fingertable_data(State, NetList, FTDict,M) ->
    [First | Rest] = NetList,
    Tables = tableData(First, State,M,[], 0),
    fingertable_data(State, Rest, dict:store(element(1, First), Tables, FTDict), M).



fingertable_send(State,M) ->
    Tables = fingertable_data(State, dict:to_list(State), dict:new(),M),
    sendFingerTabletoNode(dict:fetch_keys(Tables), State, Tables).



tableData(_, _, M,FList, M) ->
    FList;
tableData(Node, State, M, FList, I) ->
    Hash = element(1, Node),
    Successor_i = findSuccessor(Hash, State, round(math:pow(2, I)), M),
    tableData(Node, State, M, FList ++ [{Successor_i, dict:fetch(Successor_i, State)}], I + 1).




sendFingerTabletoNode([], _, _) ->
    ok;
sendFingerTabletoNode(NodesToSend, State, Tables) ->
    [First|Rest] = NodesToSend,
    Pid = dict:fetch(First ,State),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(First, Tables))},
    sendFingerTabletoNode(Rest, State, Tables).


