-module(code_reader_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-import(code_reader, [read_code/2]).

read_code_test() ->
    Bytes = [ <<2#0010:4, 2#0101:4>>, % 0010 0101
              <<2#0100:4, 2#0010:4>>, % 0100 0010
              <<2#0001:4, 2#0000:4>>, % 0001 0000
              <<2#1000:4, 2#1000:4>>, % 1000 1000
              <<2#1000:4, 2#1111:4>>  % 1000 1111
            ],

    { 1, Bytes0} = read_code(Bytes , 1),
    { 2, Bytes1} = read_code(Bytes0, 2),
    { 4, Bytes2} = read_code(Bytes1, 3),
    { 8, Bytes3} = read_code(Bytes2, 4),
    {16, Bytes4} = read_code(Bytes3, 5),
    {32, Bytes5} = read_code(Bytes4, 6),
    {64, Bytes6} = read_code(Bytes5, 7),
    { 8, Bytes7} = read_code(Bytes6, 4),
    {15, Bytes8} = read_code(Bytes7, 4),
    [<<8:4>>] = Bytes8.

read_very_long_code_test() ->
    Bytes = [ <<"\x85">>, % 1000 0101
              <<"\xc3">>, % 1100 0011
              <<"\xe7">>, % 1110 0111
              <<"\xff">>  % 1111 1111
            ],

    {1, Bytes0} = read_code(Bytes, 2),

    % 111 1110 0111 1100 0011 1000 01
    {33157345, Bytes1} = read_code(Bytes0, 25),
    [<<31:5>>] = Bytes1.

too_many_bits_requested_test() ->
    error = read_code([], 1),
    error = read_code([<<0>>], 9),
    error = read_code([<<0>>, <<0>>], 17),
    error = read_code([<<0>>, <<0>>, <<0>>], 25),

    Bytes = [<<0>>, <<0>>],
    {0, Bytes0} = read_code(Bytes , 5),
    {0, Bytes1} = read_code(Bytes0, 5),
    {0, Bytes2} = read_code(Bytes1, 5),
    error       = read_code(Bytes2, 5).

invalid_number_of_bits_requested_test() ->
    error = read_code([<<0>>],  0),
    error = read_code([<<0>>], -1),
    error = read_code([<<0>>], -2).

-endif. % TEST
