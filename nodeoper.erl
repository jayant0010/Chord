-module(nodeoper).

-export([inrange/4,nodePrevious/2,getDistance/4,closest/5]).

inrange(From, To, Key, M) ->
    if 
        From < To -> 
            (From =< Key) and (Key =< To);
        trunc(From) == trunc(To) ->
            trunc(Key) == trunc(From);
        From > To ->
            ((Key >= 0) and (Key =< To)) or ((Key >= From) and (Key < round(math:pow(2, M))))
    end.

nodePrevious(Id, NodeState) ->
    case 
        inrange(dict:fetch(id, NodeState) + 1, dict:fetch(id, dict:fetch(successor, NodeState)), Id, dict:fetch(m, NodeState)) of 
            true -> NodeState;
            _ -> nodePrevious(Id, prevFingerClosest(Id, NodeState, dict:fetch(m, NodeState)))
    end.

prevFingerClosest(_, NodeState, 0) -> NodeState;
prevFingerClosest(Id, NodeState, M) -> 
    MthFinger = lists:nth(M, dict:fetch(finger_table, NodeState)),
    
    case inrange(dict:fetch(id, NodeState), Id, dict:fetch(node ,MthFinger), dict:fetch(m, NodeState)) of
        true -> 

            dict:fetch(pid ,MthFinger) ! {state, self()},
            receive
                {statereply, fingerState} ->
                    fingerState
            end,
            fingerState;

        _ -> prevFingerClosest(Id, NodeState, M - 1)
    end.

getDistance(Key, Key, _, Distance) ->
    Distance;
getDistance(Key, NodeId, M, Distance) ->
    getDistance(Key, (NodeId + 1) rem round(math:pow(2, M)), M, Distance + 1).

%points to the closest node
closest(_, [], MinNode, _, _) ->
    MinNode;
closest(Key, FingerNodeIds, MinNode, MinVal, State) ->
    [First| Rest] = FingerNodeIds,
    Distance = getDistance(Key, First, dict:fetch(m, State), 0),
    if
        Distance < MinVal ->
            closest(Key, Rest, First, Distance, State);
        true -> 
            closest(Key, Rest, MinNode, MinVal, State)
    end.