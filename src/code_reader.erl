%% @doc A module to read variable-length codes from a byte stream.

-module(code_reader).
-export([read_code/2]).

%% @doc Reads an N-bit code from the given list of bytes, where the requested
%% code may span multiple bytes.
%%
%% A single code may be smaller than one byte. A code may also span more than
%% one byte, by taking a few bits from one byte and the remaining bits from the
%% subsequent bytes.
%%
%% For example, given the bytes:
%%
%%   0110 0100   0010 1010
%%
%% When a 5-bit code is read the code would be the least significant
%% bits of the first byte, i.e. `00100` = `4`. This leaves the pattern:
%%
%%    011   0010 1010
%%
%% The next 5-bit code is `10 011`, where `011` comes from the first byte and
%% `10` comes from the second.
%%
%% @param  Bytes  the byte stream from which to read. Each entry in this list
%% is a bitstream representing a single byte in the stream, with the exception
%% of the first entry. The first entry is _at most_ a byte, but may be fewer
%% than 8 bits, if some of the bits in that byte were read already via a
%% previous call to `read_code/2`.
%%
%% @param N  the number of bits to read from the byte stream. It is an error to
%% request 0 or fewer bits, or to request more bits than are contained in the
%% byte stream.
%%
%% @returns  `error` when an invalid number of bits are requested
%% @returns  a tuple consisting of:
%%   - The code that was read. Returned as an integer.
%%   - The remaining byte stream after the requested code has been consumed.
%%     This byte stream is suitable for passing back into `read_code/2`.
read_code(    _, N) when N =< 0 -> error;
read_code(Bytes, N) -> read_code(Bytes, N, 0, 0).

%% @doc Reads an N-bit code from the given list of bytes, where the requested
%% code may span multiple bytes. A helper function for the main `read_code/2`
%% function, with the following additional arguments:
%%
%% @param SoFar  the part of the code that has been read so far. This portion
%% represents the least significant part of the code, coming from earlier bytes
%% in the byte stream.
%%
%% @param BitsReadSoFar  the number of bits that have already been read. This
%% number cannot be inferred from the value that has been read so far, because
%% the value read so far may have leading zeros that are not apparent in the
%% numerical representation of the partial code.
%%
%% An invariant of this function is that `N > 0`.
%%
%% The return value of this function can be directly returned by `read_code/2`.

%% Case: running out of bytes to read is an error. Because of the invariant
%% that `N > 0`, we can be sure something data is being requested from the
%% empty byte stream, hence the error.
read_code([], _, _, _) -> error;

%% Case: the requested number of bits fits into the first entry in the byte
%% stream, with more bits to spare.
read_code([First|Rest], N, SoFar, BitsReadSoFar) when bit_size(First) > N ->
    % In that case, read the `N` least significant bits, leaving the remaining
    % bits. These remaining bits form the new first byte of the byte stream.
    RestOfFirstLength = bit_size(First) - N,
    <<RestOfFirst:RestOfFirstLength, Code:N>> = First,

    {
     composed_code(Code, SoFar, BitsReadSoFar),
     [<<RestOfFirst:RestOfFirstLength>>|Rest]
    };

%% Case: the requested number of bits is exactly the number of bits in the
%% first entry in the byte stream, with no bits to spare.
read_code([First|Rest], N, SoFar, BitsReadSoFar) when bit_size(First) == N ->
    % In that case, just read the entire first entry as a number. The new byte
    % stream is the remaining entries in the input byte stream.
    L = bit_size(First),
    <<Code:L/integer>> = First,  % read as number

    % In particular, don't recurse further. This is important to enforce the
    % invariant that `N > 0` in all calls.
    {composed_code(Code, SoFar, BitsReadSoFar), Rest};

%% Case: even after reading the first entry in the byte stream, more bits have
%% to be read.
read_code([First|Rest], N, SoFar, BitsReadSoFar) ->
    % Start by reading the first entry in the byte stream as a number.
    L = bit_size(First),
    <<Code:L/integer>> = First,  % read as number

    % Compose that number with whatevre part of the code has been read so far.
    NewSoFar = composed_code(Code, SoFar, BitsReadSoFar),

    % Finally, read the remaining of the bits from the remaining entries in the
    % byte stream, accounting for the fact that a certain number of additional
    % bits has been read.
    read_code(Rest, N - L, NewSoFar, BitsReadSoFar + L).

%% @doc Compose a partially read code with an additional part of the code that
%% was just read.
%%
%% @param NewPart  the part of the code that was most recently read. This
%% portion represents the most significant part of the code (up to this point,
%% as parts of the code that are yet to be read will represent more significant
%% parts).
%%
%% @param SoFar  the part of the code that was previously read. This portion
%% represents the least significant part of the code, coming from earlier bytes
%% in the byte stream.
%%
%% @param BitsReadSoFar  the number of bits that have already been read. This
%% number cannot be inferred from the value that has been read so far, because
%% the value read so far may have leading zeros that are not apparent in the
%% numerical representation of the partial code.
%%
%% @returns  the new partial code, consisting of both parts of the code given
%% to this function.
composed_code(NewPart, SoFar, BitsReadSoFar) ->
    (NewPart bsl BitsReadSoFar) bor SoFar.
