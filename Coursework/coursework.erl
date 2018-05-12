-module(coursework).
-export([main/0, threadManager/4, processFile/3]).


main() ->
  % Read input.txt and get the values of g, k and files names.
  {ok, Binary} = file:read_file("input.txt"),
  InputContent = string:tokens(erlang:binary_to_list(Binary), "\n"),
  G = list_to_integer(lists:nth(1, InputContent)),
  K = list_to_integer(lists:nth(2, InputContent)),
  FilesNames = lists:nthtail(2, InputContent),
  FilesAmount = length(FilesNames),
  Manager_PID = spawn(coursework, threadManager, [self(), #{}, FilesAmount, 0]),
  [spawn(coursework, processFile, [Manager_PID, FileName, G]) || FileName <- FilesNames],
  receive
    {finished, Results, TotalLines} ->
      [ResultFst | ResultOth] = Results,
      FunReduce = fun(A, B) ->
        maps:fold(fun(Key, Val, Map) -> maps:update_with(Key, fun(X) -> X + Val end, Val, Map) end, A, B) end,
      Consolidation = maps:to_list(lists:foldl(FunReduce, ResultFst, ResultOth)),
      KFirst = lists:sublist(lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, Consolidation), K),
      % Write the output file
      {ok, Output} = file:open("output.txt", write),
      lists:foreach( fun(Pair) -> prettyPrint(Output, Pair, TotalLines) end, KFirst)
  end,
  ok.


% Print a line in the asked format
prettyPrint(Output, {[PairFst | PairSnd], Frequency}, TotalLines) ->
  io:format(Output, "~c ~s ~p ~p\n",[PairFst, PairSnd, Frequency, TotalLines]).


% This thread receives and consolidates the results from the other file processor threads.
threadManager(From, Map, FilesAmount, TotalLines) ->
  receive
    {processed, Filename, Result, LinesNumber} ->
      io:format("File processed: ~p (~p lines)~n", [Filename, LinesNumber]),
      UpdatedMap = maps:put(Filename, Result, Map),
      case maps:size(UpdatedMap) < FilesAmount of
        true ->
          threadManager(From, UpdatedMap, FilesAmount, TotalLines + LinesNumber);
        false ->
          Results = maps:values(UpdatedMap),
          From ! {finished, Results, TotalLines + LinesNumber}
      end
  end.


% Processes an individual file getting the pairs map and amount of lines
processFile(From, Filename, Gap) ->
  {ok, Binary} = file:read_file(Filename),
  FileLines = string:tokens(erlang:binary_to_list(Binary), "\n"),
  ResultsMap = getPairsFromFile(Gap, FileLines),
  LinesNumber = length(FileLines),
  From ! {processed, Filename, ResultsMap, LinesNumber}.


% Get a list of tuples with the frequency of each pair with Gap for the Lines of a file
getPairsFromFile(Gap, Lines) ->
  LinesPairs = [getPairsWithGapFromStr(Gap, Line) || Line <- Lines],
  AllPairs = lists:concat(LinesPairs),
  ReduceFnt = fun(Pair, Accumulator) ->
    N = maps:get(Pair, Accumulator, 0),
    maps:put(Pair, N + 1, Accumulator) end,
  lists:foldl(ReduceFnt, #{}, AllPairs).


% With maximum Gap between chars, generates all pairs of chars from lowercased Line,
% considering only chars in US ASCII table.
% (I decided to ignore pairs in which there is 'space' char)
getPairsWithGapFromStr(Gap, Line) ->
  Tails = lists:droplast(tails(string:lowercase(Line))),
  [[X] ++ [Y] ||
    [X | YS] <- Tails,
    Y <- YS,
    X > 32, X < 127, Y > 32, Y < 127, % US ASCII chars, except space
    string:chr(YS, Y) =< Gap + 1].


% Get all tails of a line. For "abc", returns ["abc", "bc", "c"]
tails([]) -> [];
tails(Line) ->
  [Line] ++ tails(lists:nthtail(1, Line)).
