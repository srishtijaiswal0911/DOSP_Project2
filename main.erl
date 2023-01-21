-module(main).
-author("srishti").

%% API
-export([start/0]).
-export([init_topology/3,  grid_view/2, grid_view/4, imperfect_view/2, imperfect_view/5 ,verify_convergence_condition/2]).


start() ->
  {_, [Actor]} = io:fread("Number of Nodes to enter: ", "~d"),
  {_, [Topology]} = io:fread("Enter topology type [line/2d/full/imperfect3d]: ", "~s"),
  {_, [Algorithm]} = io:fread("Algorithm to be utilized [gossip/pushsum]:  ", "~s"),
  io:format("Actors ~w, Topology ~p, Algorithms ~p ~n", [Actor, Topology, Algorithm]),

  Convergence_Pid = spawn(?MODULE, verify_convergence_condition, [Actor, erlang:system_time(millisecond)]),
  register(convergence, Convergence_Pid),


  if
    Algorithm  == "gossip"->
      io:fwrite("Starting Gossip ~n"),
      List_of_actors = [spawn(gossip_algo, spawn_actors, [10]) || _ <- lists:seq(1, Actor)];

    Algorithm == "pushsum" ->
      List_of_actors = [spawn(push_sum_algo, spawn_actors, [Idx, 1, 0]) || Idx <- lists:seq(1, Actor)];

    true -> List_of_actors =[]
  end,



  NeighbourList = init_topology(Actor, Topology, List_of_actors),

  Index = rand:uniform(Actor),

  if
    Algorithm  == "gossip"->
      io:format("Reached Here"),
      Initial_Gossip_Pid = lists:nth(Index, List_of_actors),
      Pass_to_Neighbours_Pid = spawn(gossip_algo, pass_rumor_to_neighbours, []),
      register(pass_rumor_to_neighbours, Pass_to_Neighbours_Pid),
      Initial_Gossip_Pid ! {"Rumor_Message", Index, NeighbourList, List_of_actors, self()};

    Algorithm == "pushsum" ->
      Initial_Path_Sum_Pid = lists:nth(Index, List_of_actors),
      Pass_to_Neighbours_Pid = spawn(push_sum_algo, pass_rumor_to_neighbours, []),
      register(pass_rumor_to_neighbours, Pass_to_Neighbours_Pid),
      Initial_Path_Sum_Pid ! {"Push_Sum", Index, NeighbourList, List_of_actors, self(), 0, 0};

    true -> do_nothing
  end,

  io:format("Process Initiated").


verify_convergence_condition(0, Start_Time) ->
  End_Time = erlang:system_time(millisecond),
  io:format("Is server alive? ~p and PID: ~p ~n", [is_process_alive(self()), self()]),
  io:format("Start Time ~w", [Start_Time]),
  io:format("End Time ~w", [End_Time]),
  io:format("Convergence time is ~w milliseconds  ~n", [End_Time - Start_Time]),
  erlang:halt(0);



verify_convergence_condition(Actor, Start_Time) ->
  receive {"Actor completed its Work", Pid} ->
    Pid,
    verify_convergence_condition(Actor - 1, Start_Time)
  end.

init_topology(Actors, Topology, List_of_actors) ->
  Neighbour_List = create_neighbours(List_of_actors, Topology, Actors),
    io:format("Actor list is ~p ~n and neighbor list is ~p ~n", [List_of_actors, Neighbour_List]),
  Neighbour_List.

create_neighbours(List_of_actors, Topology, Actors) ->
  io:format("creating neighbours for actors ~p  and Topology is ~p ~n", [Actors, Topology]),
  if
    Topology == "full" ->
      Neighbours = full_network(Actors, List_of_actors);
  %io:format("Full Network Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, List_of_actors]);

    Topology == "line" ->
      Neighbours = line(Actors, List_of_actors);
  %io:format("Line Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, List_of_actors]);

    Topology == "2d" ->
      Neighbours = grid_view(Actors, List_of_actors);
  %io:format("2D Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, List_of_actors]);

    Topology == "imperfect3d" ->
      Neighbours = imperfect_view(Actors, List_of_actors);
      %io:format("2D Neighbour array is ~p and Actor array is ~p ~n", [Neighbours, List_of_actors]);
    true -> invalid_topology, Neighbours = []
  end,
  Neighbours.






%%% Full 

full_network(Actors, List_of_actors) ->
  full_network(Actors, List_of_actors, []).

full_network(0, List_of_actors, Neighbours) ->
  List_of_actors,
  lists:reverse(Neighbours);

full_network(Index, List_of_actors, Neighbours) ->
  ActorNeighbours = List_of_actors -- [lists:nth(Index, List_of_actors)],
  full_network(Index - 1, List_of_actors, lists:append(Neighbours, [ActorNeighbours])).

%%% Line Topology

line(Actors, List_of_actors) ->
  line(Actors, Actors, List_of_actors, []).

line(0, Actors, List_of_actors, Neighbours) ->
  List_of_actors, Actors,
  lists:reverse(Neighbours);

line(Index, Actors, List_of_actors, Neighbours) ->
  if
    Index == 1 ->
      Next_Elem = lists:nth(Index + 1, List_of_actors),
      line(Index - 1, Actors, List_of_actors, lists:append(Neighbours, [[Next_Elem]]));

    Index == Actors ->
      io:format("Index ~p, Actors ~p, Length ~p Neighbours ~p ~n", [Index, Actors, length(List_of_actors), Neighbours]),
      Prev_Elem = lists:nth(Index - 1, List_of_actors),
      line(Index - 1, Actors, List_of_actors, lists:append(Neighbours, [[Prev_Elem]]));
    true ->
      Next_Elem = lists:nth(Index + 1, List_of_actors),
      Prev_Elem = lists:nth(Index - 1, List_of_actors),
      line(Index - 1, Actors, List_of_actors, lists:append(Neighbours, [[Prev_Elem, Next_Elem]]))
  end.


%%% Grid Representation Topology
populate_grid(Idx, Rows, Actors, List_of_actors, RowElem, Matrix) ->

  if
    Idx > Actors -> Matrix;
    true ->
      Ele = lists:nth(Idx, List_of_actors),
      TempRow = lists:append(RowElem, [Ele]),
      if
        Idx rem Rows == 0 ->
          populate_grid(Idx + 1, Rows, Actors, List_of_actors, [], lists:append(Matrix, [TempRow]));
        true ->
          populate_grid(Idx + 1, Rows, Actors, List_of_actors, TempRow, Matrix)
      end
  end.

%%% Grid View

grid_view(Actors , List_of_actors) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  populate_grid(1, Rows, Actors, List_of_actors, [], []),
  grid_view(Actors, Grid_Matrix, Rows, []).


grid_view(0, Grid_Matrix, Rows, Neighbours) ->
  Grid_Matrix, Rows,
  lists:reverse(Neighbours);

grid_view(Index, Grid_Matrix, Rows, Neighbours) ->

  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Elem_col = Rows,
      Elem_rows  = round(Index / Rows);
    true ->
      Elem_col = Index rem Rows,
      Elem_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  if
    Elem_rows == 1 ->
      if
        Elem_col == 1 ->
          E1= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E2= lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2]]));
        Elem_col == Rows ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          E2= lists:nth(Elem_col - 1, lists:nth(Elem_rows , Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2]]));
        true ->
          E1 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E3 = lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3]]))
      end;
    Elem_rows == Rows ->
      if
        Elem_col == 1 ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2]]));
        Elem_col == Rows ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2]]));
        true ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3]]))
      end;
    true ->
      if
        Elem_col == 1 ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3]]));
        Elem_col == Rows ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3]]));
        true ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          E4 = lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          grid_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3, E4]]))
      end
  end.


%%% Imperfect 3D

imperfect_view(Actors , List_of_actors) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  populate_grid(1, Rows, Actors, List_of_actors, [], []),
  imperfect_view(Actors, Grid_Matrix, Rows, [], List_of_actors).


imperfect_view(0, Grid_Matrix, Rows, Neighbours, List_of_actors) ->
  Grid_Matrix, Rows, List_of_actors,
  lists:reverse(Neighbours);

imperfect_view(Index, Grid_Matrix, Rows, Neighbours, List_of_actors) ->

  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Elem_col = Rows,
      Elem_rows  = round(Index / Rows);
    true ->
      Elem_col = Index rem Rows,
      Elem_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  Ele =  lists:nth(Elem_col, lists:nth(Elem_rows, Grid_Matrix)),
  if
    Elem_rows == 1 ->
      if
        Elem_col == 1 ->
          E1= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E2= lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2,lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), List_of_actors);
        Elem_col == Rows ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          E2= lists:nth(Elem_col - 1, lists:nth(Elem_rows , Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), List_of_actors);
        true ->
          E1 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E3 = lists:nth(Elem_col, lists:nth(Elem_rows + 1, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, E3, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), List_of_actors)
      end;
    Elem_rows == Rows ->
      if
        Elem_col == 1 ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), List_of_actors);
        Elem_col == Rows ->
          E1= lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), List_of_actors);
        true ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, E3, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), List_of_actors)
      end;
    true ->
      if
        Elem_col == 1 ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, E3, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), List_of_actors);
        Elem_col == Rows ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, E3, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), List_of_actors);
        true ->
          E1 = lists:nth(Elem_col, lists:nth(Elem_rows - 1, Grid_Matrix)),
          E2= lists:nth(Elem_col , lists:nth(Elem_rows + 1, Grid_Matrix)),
          E3 = lists:nth(Elem_col - 1, lists:nth(Elem_rows, Grid_Matrix)),
          E4 = lists:nth(Elem_col + 1, lists:nth(Elem_rows, Grid_Matrix)),
          Rem_List = List_of_actors -- [E1, E2, E3, E4, Ele],
          imperfect_view(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[E1, E2, E3, E4,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), List_of_actors)
      end
  end.







