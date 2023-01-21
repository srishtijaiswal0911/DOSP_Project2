-module(gossip_algo).
-author("srishti").

%% API
-export([spawn_actors/1, pass_rumor_to_neighbours / 0, index_of/2]).

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).



spawn_actors(Counter)->
  receive {"Rumor_Message", Index, List_of_neighbours, List_of_actors, Parent} ->
    Parent,
    if
      Counter == 1 ->
        io:format("Convergence was attained for ~p ~n", [self()]),
        convergence ! {"Actor completed its Work", self()},
        pass_rumor_to_neighbours ! {"No longer able to acknowledge messages", "Dead", Parent, List_of_actors, List_of_neighbours, self()},
        exit(normal);
      true ->
        pass_rumor_to_neighbours ! {Index, List_of_neighbours, List_of_actors, self()},
        spawn_actors(Counter - 1)
    end
  end.

pass_rumor_to_neighbours() ->
  receive
    {Index, List_of_neighbours, List_of_actors, PID} ->
      Neighbours = lists:nth(Index, List_of_neighbours),
      Len = length(Neighbours),

      Rand_index = rand:uniform(Len),
      Neighbour_Pid = lists:nth(Rand_index, Neighbours),
      Is_Alive = is_process_alive(Neighbour_Pid),

        if
          Is_Alive == false ->
          io:format("Neighbour ~p for node ~p is dead  ~n", [Neighbour_Pid, PID]),
          self() ! {Index, List_of_neighbours, List_of_actors, PID},
          pass_rumor_to_neighbours();
        true ->
          Neighbour_Pid ! {"Rumor_Message", index_of(Neighbour_Pid, List_of_actors), List_of_neighbours, List_of_actors, PID},
          self() ! {Index, List_of_neighbours, List_of_actors, PID},
          pass_rumor_to_neighbours()

      end;

    {"No longer able to acknowledge messages", State , PID, List_of_actors, List_of_neighbours, Neighbour_Pid} ->
      State, Neighbour_Pid,

      self() ! {index_of(PID, List_of_actors), List_of_neighbours, List_of_actors, PID},
      pass_rumor_to_neighbours()

  end.

