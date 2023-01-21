
-module(push_sum_algo).
-author("srishti").

%% API
-export([spawn_actors/3, pass_rumor_to_neighbours / 0, index_of/2]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).


spawn_actors(S, W, Counter)->
    receive {"Push_Sum", Index, List_of_neighbours, List_of_actors, Parent, Received_S, Received_W} ->
        if
           Counter > 3 ->

               io:format("Convergence was attained for ~p ~n", [self()]),
               convergence ! {"Actor completed its Work", self()},
               pass_rumor_to_neighbours ! {"No longer able to acknowledge messages", "Dead", Parent, List_of_actors, List_of_neighbours, self(), Received_S, Received_W},
               exit(normal);
            true ->
                Old_Ratio = S / W,
%%                io:format("Initial Ratio is ~p ~n", [Old_Ratio]),
                New_S = S + Received_S,
                New_W = W + Received_W,
                New_Ratio = New_S / New_W,
%%                io:format("Initial Ratio is ~p ~n", [New_Ratio]),
                Diff = abs(New_Ratio - Old_Ratio),
                Pow = math:pow(10, -2),
                if
                    Diff < Pow ->
                        NewCounter = Counter + 1;
                    true -> NewCounter = 0
                end,

                pass_rumor_to_neighbours ! {Index, List_of_neighbours, List_of_actors, self(), New_S / 2, New_W / 2},
                spawn_actors(New_S / 2, New_W / 2, NewCounter)
        end

    end.


pass_rumor_to_neighbours() ->
    receive
        {Index, List_of_neighbours, List_of_actors, PID, S, W} ->
            Neighbours = lists:nth(Index, List_of_neighbours),
            Len = length(Neighbours),
            Arbitrary_index = rand:uniform(Len),
            Pid_of_neighbour = lists:nth(Arbitrary_index, Neighbours),
            Is_Alive = is_process_alive(Pid_of_neighbour),
            if
                Is_Alive == false ->
                    self() ! {Index, List_of_neighbours, List_of_actors, PID, S, W},
                    pass_rumor_to_neighbours();
                true ->
%%                    io:format("Message from ~p to ~p ~n", [PID,Pid_of_neighbour]),
                    Pid_of_neighbour ! {"Push_Sum", index_of(Pid_of_neighbour, List_of_actors), List_of_neighbours, List_of_actors, PID, S, W},
                    self() ! {Index, List_of_neighbours, List_of_actors, PID, S, W},
                    pass_rumor_to_neighbours()
            end;

        {"No longer able to acknowledge messages", State , PID, List_of_actors, List_of_neighbours, Pid_of_neighbour, S, W} ->
            State, Pid_of_neighbour,
            self() ! {index_of(PID, List_of_actors), List_of_neighbours, List_of_actors, PID, S, W},
            pass_rumor_to_neighbours()

    end.

