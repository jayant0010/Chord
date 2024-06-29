-module(controller).

-import(main,[near_node/3]).
-export([listen/2,hopCount/0]).

hopCount() ->
    receive
        {totalhops, HCount} ->
            HCount
        end.

listen(State,M) ->
    Hash = dict:fetch(id, State),
    receive
            
    {lookup, Id, Key, HCount, _Pid} ->

             NodeVal = near_node(Key, dict:fetch_keys(dict:fetch(finger_table ,State)), State),
            NewState = State,
            if 
                    
                (Hash == Key) -> 
                    taskcompletionmonitor ! {completed, Hash, HCount, Key};
                (NodeVal == Key) and (Hash =/= Key) -> 
                    taskcompletionmonitor ! {completed, Hash, HCount, Key};
                    
                true ->
                    dict:fetch(NodeVal, dict:fetch(finger_table, State)) ! {lookup, Id, Key, HCount + 1, self()}
            end
            ;
    {kill} ->
        NewState = State,
        exit("execution complete");
    {state, Pid} -> Pid ! State,
                    NewState = State;
  
    {fix_fingers, Table} -> 
        %io:format("\nFinger Table ~p ~p\n", [Hash, Table]),
        NewState = dict:store(finger_table, Table, State)
    end, 
listen(NewState,M).



