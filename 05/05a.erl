#!/usr/bin/env escript

main(_) ->
  {ok, File} = file:open("input.txt", read),
  try 
    Lines = get_all_lines(File),
    MaxSeatId = lists:foldl(fun get_max_seat_id/2, -1, Lines),
    io:format('~B', [MaxSeatId])
  after 
    file:close(File)
  end.

get_all_lines(File) ->
  case io:get_line(File, "") of
    eof  -> [];
    Line -> [Line] ++ get_all_lines(File)
  end.

get_max_seat_id(Line, MaxSeatId) ->
  SeatId = compute_seat_id(string:trim(Line)),
  case SeatId > MaxSeatId of
    true  -> SeatId;
    false -> MaxSeatId
  end.

compute_seat_id(Line) ->
  BitSequence = [seat_char_to_boolean(Char) || Char <- Line],
  list_to_integer(BitSequence, 2).

seat_char_to_boolean(Char) ->
  case Char of
    $B -> $1;
    $F -> $0;
    $R -> $1;
    $L -> $0
  end.
