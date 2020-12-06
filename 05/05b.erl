#!/usr/bin/env escript

main(_) ->
  {ok, File} = file:open("input.txt", read),
  try 
    Lines = get_all_lines(File),
    MissingSeatId = get_missing_seat_id(Lines),
    io:format('~B', [MissingSeatId])
  after 
    file:close(File)
  end.

get_all_lines(File) ->
  case io:get_line(File, "") of
    eof  -> [];
    Line -> [Line] ++ get_all_lines(File)
  end.

get_missing_seat_id(Lines) ->
  SeatIds = [compute_seat_id(string:trim(Line)) || Line <- Lines],
  SortedSeatIds = lists:sort(SeatIds),
  find_seat_id_gap(SortedSeatIds).

find_seat_id_gap([SeatId|RemainingSeatIds]) -> find_seat_id_gap(RemainingSeatIds, SeatId).

find_seat_id_gap([SeatId|RemainingSeatIds], PrevSeatId) ->
  case SeatId == PrevSeatId + 1 of
    true  -> find_seat_id_gap(RemainingSeatIds, SeatId);
    false -> SeatId + 1
  end.

compute_seat_id(Line) ->
  TrimmedLine = string:trim(Line),
  BitSequence = [seat_char_to_boolean(Char) || Char <- TrimmedLine],
  list_to_integer(BitSequence, 2).

seat_char_to_boolean(Char) ->
  case Char of
    $B -> $1;
    $F -> $0;
    $R -> $1;
    $L -> $0
  end.
