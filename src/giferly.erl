%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% giferly - a GIF 89a decoder
%   - by Avik Das <avikstrange@gmail.com>
% 
% A standalone program to decode and display a GIF file. This is my attempt to
% learn both the GIF 89a format and Erlang. This project started by my
% encountering an article describing the GIF format, in essence as a summary of
% some of the more fundamental parts of the official specification of the
% format.  While this task would have been fairly straightforward in Ruby, C or
% some other language I am familiar with, I decided to implement the decoder in
% Erlang in order to learn the language.
% 
% This program only depends on Esdl, an Erlang binding to the Simple
% DirectMedia Layer, to display the decoded image. Other than that, the entire
% parsing is implemented from scratch in Erlang. While Erlang may not
% necessarily be the ideal choice for this task, it is an interesting exercise
% to attempt this in a functional style, and taking advantage of Erlang's
% binary data manipulation features.
% 
% References:
%  * The article that piqued my interest:
%    - http://matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp
%  * The official specification of the GIF 89a format:
%    - http://www.w3.org/Graphics/GIF/spec-gif89a.txt
%  * The book I'm using to learn Erlang:
%    - http://learnyousomeerlang.com/contents
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(giferly).
-author('Avik Das <avikstrange@gmail.com>').
-export([go/1]).

-include_lib("esdl/include/sdl.hrl").
-include_lib("esdl/include/sdl_events.hrl").
-include_lib("esdl/include/sdl_video.hrl").
-include_lib("esdl/include/sdl_keyboard.hrl").

% == GIF FILE FORMAT ==========================================================

% ~~ HEADER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% The first six bytes form the header, with all the bytes representing ASCII
% characters. First, the header must contain "GIF" as the first three bytes,
% followed by the version of the specification used to encode this image. The
% only version supported by this library is "89a", with the older "87a" not
% readily found anymore.
header_valid(<<"GIF89a">>) ->
    true;
header_valid(_) ->
    false.

% ~~ LOGICAL SCREEN DESCRIPTOR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% The first four bytes of the logical screen descriptor specify the canvas
% width and height. Each of the values takes up two bytes and is specified with
% the least significant byte first.
screen_dim(<<W:16/little, H:16/little, _:24>>) ->
    {{w, W}, {h, H}}.

% The next byte is a packed byte containin multiple pieces of information.
% The first bit is the global color table flag, with "1" signifying that there
% is a global color table, and the table will follow the logical screen
% descriptor.
global_color_table_flag(<<_:32, 1:1, _:23>>) ->
    true;
global_color_table_flag(<<_:32, 0:1, _:23>>) ->
    false.

% The next three bits state the color depth, minus one. Thus, 2#001 signifies a
% color depth of 2BPP, while 2#111 signifies a color depth of 8BPP.
color_depth(<<_:32, _:1, Depth:3, _:20>>) ->
    Depth + 1.

% The next bit is the sort flag, with "1" signifying that the colors in the
% global color table are sorted by "decreasing importance." While this can be
% used by the decoder, it can also be safely ignored, as we will do.

% The final three bits specify the size of the global color table as calculated
% by the the formula: size = 2^(N+1), where N is the value of the three bits in
% question. The size reported is the number of colors in the table.
global_color_table_size(<<_:32, _:5, N:3, _:16>>) ->
    round(math:pow(2, N + 1)).

% The next byte of the logical screen descriptor is the background color index,
% and is only meaningful when the global color table flag is "1". In this case,
% the background color index specifies which color in the global color table is
% usedwhen a pixel does not specify a value in the image data. If there is no
% global color table, this byte should be "0".
background_color_index(<<_:40, Index:8, _:8>>) ->
    Index.

% The final byte of the logical screen descriptor is the pixel aspect ratio.
% This will be ignored as well, since it is typically set to "0".

% ~~ GLOBAL COLOR TABLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-record(color, {r, g, b}).

% The global color table immediately follows the logical screen descriptor if
% the global color table flag is set. If so, then the size of the color table,
% also in the logical screen descriptor, specifies the number of colors in the
% table. Each color takes up three bytes, representing the red, green and blue
% components respectively.

color_table(_, 0, ParsedColors) ->
    lists:reverse(ParsedColors);

color_table(BinData, NumColorsLeft, ParsedColors) ->
    <<R:8, G:8, B:8, Rest/binary>> = BinData,
    NewParsedColors = [#color{r=R, g=G, b=B}|ParsedColors],

    color_table(Rest, NumColorsLeft - 1, NewParsedColors).

% The local color table, described later, is identical in structure to the
% global color table, and so can use the same parser.
global_color_table(BinData, NumColors) ->
    color_table(BinData, NumColors, []).

% ~~ EXTENSION BLOCKS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% An number of extension blocks may appear in the data stream. All of these
% extensions are signalled by the first byte of the block, which is always
% "0x21". The approach here will be to ignore the ones that are not pertinent
% to this application, which will still require recognzing the blocks and
% skipping over the contained data.

% --- Sub-blocks --------------------------------------------------------------

% Both extension blocks and the image data are grouped into data sub-blocks.
%
% Each sub-block starts with a single byte stating the number of bytes of data
% in the following sub-block, followed by that many bytes of data. Once a
% sub-block with size zero is reached, no more sub-blocks follow.

%% @doc Reads all the consecutive sub-blocks starting at the beginning of the
%% given bit string.
%%
%% @returns  All the binary data in the sub-blocks, as a single bit string.
next_sub_block(<<   0:8, Rest/binary>>, SubBlocks) ->
    {SubBlocks, Rest};
next_sub_block(<<Size:8, Rest/binary>>, SubBlocks) ->
    <<Block:Size/bytes, RemainingSubBlocks/binary>> = Rest,
    SubBlocksNew = <<SubBlocks/binary, Block/binary>>,
    next_sub_block(RemainingSubBlocks, SubBlocksNew).

% --- Graphics Control Extension ----------------------------------------------

% Most of the extensions are handled in the main parse routines. However, some
% of the functionality in one of the extensions--the graphics control
% extension--is specified below.

%% @doc A temporary holding place for the information in the graphics control
%% extension. This information is later attached directly to the image
%% immediately following this extension block.
-record(graphic_control, {delay, disposal, transparency}).

% The first byte in the graphics control extension is a packed byte, the first
% three bits of which are reserved. Following this are three bits denoting the
% disposal method, which specifies how the decoder should handle the canvas
% after a image has been displayed, before displaying the next. This is used
% for animating multiple frames one after another. The possible values are:

% 0   - No disposal specified. This is used when there is no animation. The
%       decoder should do nothing.
-define(DISPOSAL_UNSPECIFIED , 0).

% 1   - Do not dispose. Leave the canvas as it is, painting over it for the
%       next frame. This allows for incremental changes between frames.
-define(DISPOSAL_NONE        , 1).

% 2   - Restore to background color. The area belonging to the current image
%       should be completely painted with the background color, which is
%       specified in the logical screen descriptor.
-define(DISPOSAL_RESTORE_BG  , 2).

% 3   - Restore to previous. The area belonging to the current image should be
%       restored to whatever was present in that area before.
-define(DISPOSAL_RESTORE_PREV, 3).

% 4-7 - To be defined.

disposal_method(<<_:3/bits, DisposalMethod:3, _:2/bits>>) ->
    DisposalMethod.

% The next bit in the packed byte is the user input flag. Determines whether
% the application is expected to wait for some user input (as defined by the
% application) before displaying the next image. It is unlikely that this will
% be set, and this application will ignore it.

% The final bit is the transparency flag. If set, then the transparency index,
% given later in the graphics control extension, should be used to provide
% transparency in the image.
transparency_flag(<<_:7/bits, 1:1>>) ->
    true ;
transparency_flag(<<_:7/bits, 0:1>>) ->
    false.

% ~~ IMAGE DESCRIPTOR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% A single GIF file may contain multiple images (this is used in animated GIFs)
% and ech such image begins with an image descriptor block. The first byte of
% the image descriptor is the image separotor with a value of "2C". This will
% be matched by all of the functions below.

% A single image need not take up the entire canvas, so the first two bytes and
% the second two bytes specify the left offset and the top offset respectively.
% The third two bytes and the fourth two bytes are the width and the height
% respectively. Each is specified with the least significant byte first.
image_descriptor_dim(<<16#2c,
    L:16/little, T:16/little,
    W:16/little, H:16/little,
    _:8>>) -> {{l, L}, {t, T}, {w, W}, {h, H}}.

% The last byte of the image descriptor is a packed field. The first bit is the
% local color table flag, which is "1" when the following image data has a
% local color table immediately following the image descriptor.
local_color_table_flag(<<16#2c, _:64/bits, 1:1, _:7>>) ->
    true;
local_color_table_flag(<<16#2c, _:64/bits, 0:1, _:7>>) ->
    false.

% The second bit is the interlace flag.
% TODO?

% The third flag is the sort flag, which functions just like in the case of the
% logical screen descriptor, with a value of "1" stating that the local color
% table is sorted by "decreasing importance". Again, this can be ignored, as we
% will do.

% The fourth and fifth bits are reserved.

% The last three bits specify the size of the local color table. Again, the
% size is calculated by the the formula: size = 2^(N+1), where N is the value
% of the three bits in question. The size reported is the number of colors in
% the table.
local_color_table_size(<<16#2c, _:69/bits, N:3>>) ->
    round(math:pow(2, N + 1)).

% The local color table immediately follows the image descriptor if the local
% color table flag is set and is identical in structure to the global color
% table. So can use the same parser.
local_color_table(BinData, NumColors) ->
    color_table(BinData, NumColors, []).

% ~~ IMAGE DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% The image data block contains LZW-encoded data that, when decoded, determines
% the colors to display on a canvas. The first few functions below are relevant
% to the structure of the block itself, and the meanings of the pertinent terms
% are explained in the LZW compression section following those functions.

% The first byte of the image data is the LZW minimum code size.
lzw_minimum_code_size(<<Size:8, Rest/binary>>) ->
    {Size, Rest}.

%% @doc Read all consecutive sub-blocks, starting at the beginning of the given
%% bit string.
%%
%% @returns  The binary data in the consecutive sub-blocks, divided into
%% individual bit strings, one per byte.
image_data_all_sub_blocks(BinData) ->
    {AllSubBlockData, Rest} = next_sub_block(BinData, <<>>),

    AllSubBlockBytes =
        lists:map(fun(Byte) -> <<Byte>> end, binary_to_list(AllSubBlockData)),
    {AllSubBlockBytes, Rest}.

% --- LZW Decompression -------------------------------------------------------

% The GIF 89a format encodes its image data using the Variable-Length-Code LZW
% Compression, which is a variation o the Lempel-Ziv Compression algorithm. In
% this compression algorithm, patterns of colors in the raw data are identified
% by codes in a translation table. The codes are then further represented by
% variable length codes concatenated and partitioned into 8-bit bytes.

% ---- Translation Table ------------------------------------------------------

% The translation, or code, table starts with all the colors in the color table
% (local if there is one, global otherwise) currently being used. This is
% followed by two special codes, the CLEAR code and the EOI (end of
% information) code, used to signal special events in the decompression
% process. In particular, the code table is re-initialized using only these
% entries whenever the CLEAR code is encountered.

% In order to provide a level of abstraction, a family of functions are
% implemented that abstract over the actual data structure used to implement
% the code table. Underneath, we will use an orddict, which is an arbitrary
% choice due to the lack of benchmarking (since performance is not a concern
% for this decoder).

-define(MAXIMUM_CODE_SIZE, 12).

lzw_code_table_new(NumColors) ->
    Table = lzw_code_table_initialize(0, NumColors, orddict:new()),

    Table2 = lzw_code_table_add(Table , clear),
    Table3 = lzw_code_table_add(Table2, eoi  ),
    Table3.

lzw_code_table_initialize(_, 0        , Table) ->
    Table;
lzw_code_table_initialize(I, NumColors, Table) ->
    NewTable = lzw_code_table_add(Table, [I]),
    lzw_code_table_initialize(I + 1, NumColors - 1, NewTable).

lzw_code_table_add(Table, Pattern) ->
    Size = orddict:size(Table),
    orddict:store(Size, Pattern, Table).

% Returns {ok, Value}, or `error` if the key is not present in the dictionary.
% This is the actual behavior for orddict:find, but even if the underlying
% choice of data structure were to change, this function would return values in
% the same format.
lzw_code_table_lookup(Table, Code) ->
    orddict:find(Code, Table).

% Technically, a data structure used to implement the code table should grow
% dynamically, but a certain number of bits are needed to index into the code
% table if there are sufficiently many elements present. Thus, we need to check
% if the table contains the maximum number of elements that can be indexed by
% the given code size (in bits).
lzw_code_table_full(Table, CSize) ->
    orddict:size(Table) =:= round(math:pow(2, CSize)).

%% @doc Determines what the next code size should be, given the current table
%% and code size. The code size generally increases when the table is filled up
%% with the maximum number of entries addressable by the current code size.
lzw_code_table_next_code_size(Table, CSize) ->
    % One caveat is if the code table is filled up for the current code size,
    % and the code size can't increase, then the next code in the code stream
    % will be a CLEAR code. The code size does not increase in this case.
    ShouldIncreaseCCSize =
        (CSize < ?MAXIMUM_CODE_SIZE) and lzw_code_table_full(Table, CSize),

    case ShouldIncreaseCCSize of
        true -> CSize + 1;
        _    -> CSize
    end.

% ---- Data Stream Representation ---------------------------------------------

% The LZW compression algorithm converts a stream of indices into a stream of
% codes, with each code corresponding to some pattern of consecutive indices.
%
% The codes themselves are not stored as a single byte per code, as that would
% limit the number of codes to 256, as well as taking up unnecessary space for
% codes when there are fewer unique codes in the table at that point during the
% execution of the compression algorithm. Thus, the number of bits used to
% store a code varies over the execution of the algorithm.
%
% See the `code_reader` module for information about the exact representation.

% ---- Decompression Algorithm ------------------------------------------------

% The exact decompression algorithm has the following characteristics:
%
%  * Originally, the code size is the LZW Minimum Code Size plus one, since
%    the Minimum Code Size is only sufficient for a table consisting only of
%    the colors, and not the two special codes, CLEAR and EOI.
%
%  * Whenever the CLEAR code is encountered, the code table is rebuilt, and
%    the current code size goes back to the original code size. The next code
%    is immediately retrieved, and its value in the table is sent to the output
%    stream. This is because the maximum code size is 12 bits. An encoder may
%    choose to say with this code size when it is reached, not adding any more
%    patterns to the code table.
%
%  * After the first code following the CLEAR code is decoded, the following
%    loop is executed:
%
%    1. Let CODE be the next code in the code stream, and {CODE} its
%       corresponding pattern in the code table, if the code is in the table.
%       Similarly, CODE-1 is the code that has just been decoded, and {CODE-1}
%       its corresponding pattern.
%
%    2. If CODE is in the table:
%
%       a. Output {CODE} to the output stream.
%
%       b. Let K be the first index in {CODE}.
%
%       c. Add {CODE-1} ++ [K] to the code table.
%
%    3. IF CODE is not in the table:
%
%       a. Let K be the first index in {CODE-1}.
%
%       b. Output {CODE-1} ++ [K] to the output stream.
%
%       c. Add {CODE-1} ++ [K] to the code table.
%
%    4. Restart loop.
%
%  * When the code table contains the maximum number of codes addressable by
%    the current code size (i.e. after the code with index 2^{CodeSize}-1 is
%    added), the code size is increased by 1.
%
%    The exception is when the current code size is the maximum allowable code
%    size, in which case the code size remains the same, and the next code is
%    the clear code.
%
%  * When the EOI code is encountered, decompression is complete, and the loop
%    is terminated.

%% @doc The dynamic state of the decompression algorithm. Represents data
%% internal to the algorithm that changes from iteration to iteration. Does not
%% capture the actual data being decompressed.
-record(decompression_state, {
          previous_index_sequence,
          code_size,
          table
        }).

%% @doc The constant parameters driving the decompression algorithm. Does not
%% change over the execution of the algorithm.
-record(decompression_context, {num_colors, minimum_code_size}).

%% @doc Runs the decompression algorithm, one step at a time. Recursively calls
%% itself to continue the algorithm.
%%
%% @returns  A list of lists, with each inner list containing an index
%% sequence.
%%
%% The outer list is in reverse order, but the individual sequences are each in
%% the correct order. It's more efficient to build up such a list. To post-
%% process the output, reverse the outer list, then flatten it:
%%
%%     lists:flatten(lists:reverse(Decoded))
lzw_decode_single(Compressed, Decoded, State, Context) ->
    #decompression_state{
       previous_index_sequence=PrevIndexSequence,
       code_size=CodeSize,
       table=Table
    } = State,

    {Code, RestCompressed} = code_reader:read_code(Compressed, CodeSize),

    case lzw_code_table_lookup(Table, Code) of
        {ok, clear} ->
            io:format("Re-building code table~n"),

            #decompression_context{
               num_colors=NumColors,
               minimum_code_size=MinCodeSize
            } = Context,

            NewState = #decompression_state{
                previous_index_sequence=clear,
                code_size=MinCodeSize,
                table=lzw_code_table_new(NumColors)
            },

            lzw_decode_single(RestCompressed, Decoded, NewState, Context);

        {ok, eoi} -> Decoded;

        {ok, IndexSequence} ->
            NewDecoded = [IndexSequence|Decoded],

            NewTable =
                if
                    PrevIndexSequence == undefined -> Table;
                    PrevIndexSequence == clear -> Table;
                    true ->
                        FirstIndex = hd(IndexSequence),
                        NextIndexSequence = PrevIndexSequence ++ [FirstIndex],
                        lzw_code_table_add(Table, NextIndexSequence)
                end,

            NewCodeSize = lzw_code_table_next_code_size(NewTable, CodeSize),

            NewState = #decompression_state{
                previous_index_sequence=IndexSequence,
                code_size=NewCodeSize,
                table=NewTable
            },

            lzw_decode_single(RestCompressed, NewDecoded, NewState, Context);

        error ->
            FirstIndexInPreviousSequence = hd(PrevIndexSequence),
            IndexSequence =
                PrevIndexSequence ++ [FirstIndexInPreviousSequence],

            NewDecoded = [IndexSequence|Decoded],

            NewTable = lzw_code_table_add(Table, IndexSequence),
            NewCodeSize = lzw_code_table_next_code_size(NewTable, CodeSize),

            NewState = #decompression_state{
                previous_index_sequence=IndexSequence,
                code_size=NewCodeSize,
                table=NewTable
            },

            lzw_decode_single(RestCompressed, NewDecoded, NewState, Context)
    end.

lzw_decode(ImageData, LZWMinCodeSize, NumColors) ->
    Table = lzw_code_table_new(NumColors),
    CodeSize = LZWMinCodeSize + 1,

    DecodedSequences = lzw_decode_single(
      ImageData,
      [],
      #decompression_state{
          previous_index_sequence=undefined,
          code_size=CodeSize,
          table=Table
      },
      #decompression_context{
          num_colors=NumColors,
          minimum_code_size=CodeSize
      }
    ),

    lists:flatten(lists:reverse(DecodedSequences)).

% == PARSED RECORD STRUCTURE ==================================================

%% @doc A structure describing the entire GIF file.
-record(parsed_gif, {
    w, h,  % the dimensions of the full output
    color_depth, % bits per pixel
    % The global color table. Used when there is no local color table for a
    % given image. May be missing (empty) if all contained images have their
    % own local color tables.
    colors=[],
    % The individual contained images. Usually one per frame of animation.
    images=[]
}).

%% @doc A structure describing a single image within the GIF file. These images
%% are the ones that actually have pixels to be displayed.
-record(parsed_img, {
    % The dimensions of the bounding rectangle for this image. When rendering
    % animations, it's possible to paint over only part of the full canvas.
    l, t, w, h,  % Left, Top, Width, Height

    disposal, delay, % animation-related metadata

    transparency,  % index that represents a transparent pixel

    % The local color table. If present, used to determine the colors for this
    % image. Otherwise, the global color table is used.
    colors=[],

    % The decoded image data, represented as a sequence of indexes into
    % whichever color table is being used.
    data=[]
}).

% == MAIN ROUTINES ============================================================

go([Filename|RestArgs]) ->
    case file:read_file(Filename) of
        {ok, Data}      ->
            io:format("Raw Data: ~p~n", [Data]),
            ParsedData = parse_data(Data),
            init_sdl(ParsedData);
        {error, Reason} ->
            io:format("Unable to open file: ~s~n", [Reason]),
            error
    end.

% == MAIN PARSE ROUTINES ======================================================

parse_data(<<Header:6/bytes, Rest/binary>>) ->
    HeaderValid = header_valid(Header),
    if
         HeaderValid ->
            parse_logical_screen_descriptor(Rest);
        true                 ->
            io:format("Invalid header: ~p~n", [Header]),
            error
    end.

parse_logical_screen_descriptor(<<Lsd:7/bytes, Rest/binary>>) ->
    ParsedData = #parsed_gif{},

    case {screen_dim(Lsd), color_depth(Lsd)} of
        {{{w, W}, {h, H}}, ColorDepth} ->
            ParsedDataWH =
                ParsedData#parsed_gif{w=W, h=H, color_depth=ColorDepth}
    end,

    case global_color_table_flag(Lsd) of
        true  ->
            GCTableSize = global_color_table_size(Lsd),
            BGIndex     = background_color_index (Lsd),

            parse_global_color_table(Rest, ParsedDataWH,
                {{gc_table_size, GCTableSize},
                 {bg_index,      BGIndex    }});
        false ->
            parse_main_blocks(Rest, ParsedDataWH)
    end.

parse_global_color_table(BinData, ParsedData,
    {{gc_table_size, GCTableSize},
     {bg_index     , BGIndex    }}) ->
    GCTableByteLen = GCTableSize * 3,
    <<BinGCTable:GCTableByteLen/bytes, Rest/binary>> = BinData,

    GCTable = global_color_table(BinGCTable, GCTableSize),
    ParsedDataGC = ParsedData#parsed_gif{colors=GCTable},

    % TODO: preserve BGIndex

    parse_main_blocks(Rest, ParsedDataGC).

%% @doc When the trailer (`0x3B`) is reached, there is no more data to parse.
parse_main_blocks(<<16#3b>>, ParsedData) ->
    io:format("Parsed image: ~p~n", [ParsedData]),
    io:format("Finished parsing image...~n"),

    % One caveat is that the images list is built up in reverse order, due to
    % efficiency. Now that all the images have been parsed, reverse their
    % order.
    #parsed_gif{images=Images} = ParsedData,
    ParsedData#parsed_gif{images=lists:reverse(Images)};

%% @doc `0x21 0xF9` marks the beginning of a graphic control extension block.
%%
%% This block, like all other extensions, contains a sub-block structure.
%% However, the block is guaranteed to contain one, fixed-length block, so the
%% entire block can be parsed in a single pattern match.
%%
%% The graphic control extension is always followed by an image. (Technically,
%% a plain text extension may follow instead, but since such extensions are
%% rare, they will be ignored.)
parse_main_blocks(
  <<
    16#21:8, % extension introducer
    16#f9:8, % graphic control
    16#04:8, % sub-block size

    % The first byte is a packed byte. See the "Graphics Control Extension"
    % section above for more information about this byte. In particular, the
    % routines to unpack this byte are defined in that section.
    Packed:8/bits,

    % When not "0", specifies the amount of time to wait before displaying the
    % next image. The value is given in 1/100ths of a second and is counted
    % starting immediately after the image is rendered.
    DelayTime:16/little,

    % This is relevant only if the transparency index is set in the packed byte
    % mentioned before. If so, any pixels with this index is unmodified when
    % rendering.
    TransparentIndex:8,

    0:8, % block terminator
    Rest/binary
  >>,
  ParsedData) ->
    DisposalMethod = disposal_method(Packed),
    ParsedTransparencyIndex = case transparency_flag(Packed) of
        true  -> TransparentIndex;
        false -> -1
    end,

    GraphicControl = #graphic_control{
        delay=DelayTime,
        disposal=DisposalMethod,
        transparency=ParsedTransparencyIndex
    },

    parse_image_descriptor(GraphicControl, Rest, ParsedData);

%% @doc `0x2c` marks the beginning of an image, starting with the image
%% descriptor.
%%
%% As described in the variant of `parse_main_blocks/2` that matches a graphic
%% control extension, an image descriptor follows a graphic control extension.
%% However, an image may not have a graphic control extension at all, so the
%% image descriptor can be considered the start of its own main block.
parse_main_blocks(BinData = <<16#2c:8, _/binary>>, ParsedData) ->
    % Because there's no associated graphic control, the following image will
    % not have any of its control parameters (such as the transparency index)
    % specified.
    GraphicControl = #graphic_control{
        delay=0,
        disposal=?DISPOSAL_UNSPECIFIED,
        transparency=-1
    },

    parse_image_descriptor(GraphicControl, BinData, ParsedData);

%% @doc `0x21 0xFE` marks the beginning of a comment extension block. All the
%% bytes in the contained sub-blocks are ASCII characters.
parse_main_blocks(<<16#21fe:16, BinData/binary>>, ParsedData) ->
    {AllSubBlockData, Rest} = next_sub_block(BinData, <<>>),

    Comment = binary_to_list(AllSubBlockData),
    io:format("Comment: ~s~n", [Comment]),

    parse_main_blocks(Rest, ParsedData);

%% @doc In general, `0x21` marks the beginning of some extension block. Any
%% extension block not handled in the above variants of `parse_main_blocks/2`
%% fall through to here, where all the sub-blocks are read and skipped.
parse_main_blocks(<<16#21:8, ExtType:8, BinData/binary>>, ParsedData) ->
    io:format("Found extension with unknown type: ~b. Ignoring~n", [ExtType]),

    {_, Rest} = next_sub_block(BinData, <<>>),
    parse_main_blocks(Rest, ParsedData).

parse_image_descriptor(
  #graphic_control{
     delay=DelayTime,
     disposal=DisposalMethod,
     transparency=TransparencyIndex
  },
  <<ImDesc:10/bytes, Rest/binary>>,
  ParsedData) ->
    {{l, L}, {t, T}, {w, W}, {h, H}} = image_descriptor_dim(ImDesc),
    ParsedImage = #parsed_img{
        l=L,
        t=T,
        w=W,
        h=H,
        disposal=DisposalMethod,
        delay=DelayTime,
        transparency=TransparencyIndex
    },

    case local_color_table_flag(ImDesc) of
        true  ->
            LCTableSize = local_color_table_size(ImDesc),
            BGIndex     = background_color_index(ImDesc),

            parse_local_color_table(Rest, ParsedData, ParsedImage,
                {{lc_table_size, LCTableSize},
                 {bg_index,      BGIndex    }});
        false ->
            parse_image_data(Rest, ParsedData, ParsedImage)
    end.

parse_local_color_table(BinData, ParsedData, ParsedImage,
    {{lc_table_size, LCTableSize},
     {bg_index     , BGIndex    }}) ->
    LCTableByteLen = LCTableSize * 3,
    <<BinLCTable:LCTableByteLen/bytes, Rest/binary>> = BinData,

    LCTable = local_color_table(BinLCTable, LCTableSize),
    ParsedImageLC = ParsedImage#parsed_img{colors=LCTable},

    % TODO: preserve BGIndex

    parse_image_data(Rest, ParsedData, ParsedImageLC).

parse_image_data(BinData, ParsedData, ParsedImage) ->
    {LZWMinCodeSize, SubBlocks} = lzw_minimum_code_size(BinData),
    NumColors = length(ParsedData#parsed_gif.colors),

    {ParsedImageData, Rest} = image_data_all_sub_blocks(SubBlocks),
    ImageDataDecoded = lzw_decode(ParsedImageData, LZWMinCodeSize, NumColors),

    OldImages = ParsedData#parsed_gif.images,
    ParsedImageFull = ParsedImage#parsed_img{data=ImageDataDecoded},
    ParsedDataIm = ParsedData#parsed_gif{images=[ParsedImageFull|OldImages]},

    parse_main_blocks(Rest, ParsedDataIm).

% == MAIN SDL ROUTINES ========================================================

% Note from Avik: I use a Hi-DPI display, and a larger zoom looks better. For
% anyone not using a Hi-DPI display, a zoom level of 2 still won't look bad.
-define(ZOOM_DEFAULT, 2).

-define(ZOOM_MIN,  2).
-define(ZOOM_MAX, 32).

-define(ZOOM_INCREASE, 1).
-define(ZOOM_DECREASE, 2).

-define(DISP_BPP, 16).

init_sdl(#parsed_gif{images=[]}) ->
    io:format("No images to display; shutting down...~n"),
    ok;
init_sdl(ParsedData) ->

    case init_video(ParsedData) of
        error ->
            sdl:quit(),
            error;
        ok    ->
            draw_image(ParsedData, [], ?ZOOM_DEFAULT),
            io:format("Shutting down...~n"),
            sdl:quit(),
            ok
    end.

init_video(#parsed_gif{w=W, h=H}) ->
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),

    Surface = resize_canvas(W, H, ?ZOOM_DEFAULT),
    % TODO: set the title to the image filename?
    sdl_video:wm_setCaption("giferly", []),

    case Surface of
        error ->
            io:format("Can't set video mode~n"),
            error;
        _     -> ok
    end.

resize_canvas(W, H, Zoom) ->
    Surface = sdl_video:setVideoMode
        (W * Zoom, H * Zoom, ?DISP_BPP, ?SDL_SWSURFACE),

    case Surface of
        error -> error;
        _     ->
            draw_background(),
            Surface
    end.

draw_background() ->
    SurfaceRef = sdl_video:getVideoSurface(),
    Surface = sdl_video:getSurface(SurfaceRef),

    #sdl_surface{w=W, h=H} = Surface,
    DestRect = #sdl_rect{x=0, y=0, w=W, h=H},

    BGColor = sdl_video:mapRGB(Surface, 255, 255, 255),
    sdl_video:fillRect(Surface, DestRect, BGColor),

    BGColor2 = sdl_video:mapRGB(Surface, 192, 192, 192),
    draw_bg_checkers(Surface, 0, 0, W, H, 4, BGColor2),

    sdl_video:updateRect(SurfaceRef, 0, 0, W, H).

draw_bg_checkers(Surface, X, Y, W, H, Size, Color) ->
    Parity = (X div Size + Y div Size) rem 2,

    if
        Y > H        -> ok;
        X > W        ->
            draw_bg_checkers(Surface, 0, Y + Size, W, H, Size, Color);
        Parity =:= 0 ->
            draw_bg_checkers(Surface, X + Size, Y, W, H, Size, Color);
        Parity =/= 0 ->
            DestRect = #sdl_rect{x=X, y=Y, w=Size, h=Size},
            sdl_video:fillRect(Surface, DestRect, Color),
            draw_bg_checkers(Surface, X + Size, Y, W, H, Size, Color)
    end.


draw_image(ParsedData, [], Zoom) ->
    #parsed_gif{images=Images} = ParsedData,
    draw_image(ParsedData, Images, Zoom);
draw_image(ParsedData, Images, Zoom) ->
    [Image|ImagesRest] = Images,

    ColorTable = case Image#parsed_img.colors of
        []         -> ParsedData#parsed_gif.colors;
        LocalTable -> LocalTable
    end,
    paint_pixels(Image, ColorTable, Zoom),

    handle_next_event(ParsedData, [Image|ImagesRest], Zoom).

handle_next_event(ParsedData, Images, Zoom) ->
    case check_event() of
        ok ->
            handle_next_event(ParsedData, Images, Zoom);
        {zoom, ZoomType} ->
            NewZoom = change_zoom(Zoom, ZoomType),
            #parsed_gif{w=W, h=H} = ParsedData,
            resize_canvas(W, H, NewZoom),
            draw_image(ParsedData, Images, NewZoom);
        next ->
            io:format("Moving to next image...~n"),

            [_|ImagesRest] = Images,
            draw_image(ParsedData, ImagesRest, Zoom);
        quit ->
            ok
    end.


change_zoom(OldZoom, ?ZOOM_INCREASE) ->
    if OldZoom >= ?ZOOM_MAX -> OldZoom;
       true                 -> OldZoom + 1
    end;
change_zoom(OldZoom, ?ZOOM_DECREASE) ->
    if OldZoom =< ?ZOOM_MIN -> OldZoom;
       true                 -> OldZoom - 1
    end.

paint_pixels(
  #parsed_img{
     l=L,
     t=T,
     w=W,
     h=H,
     transparency=TransparentIndex,
     data=Data
  },
  ColorTable,
  Zoom) ->
    Xs = lists:seq(L, L + W - 1),
    Ys = lists:seq(T, T + H - 1),

    % The decoded pixels are in row-major order. That means all the pixels for
    % a given row (i.e. a constant Y-coordinate) are consecutive. Thus, when
    % constructing the flattened list of coordinates, it's important to iterate
    % over the Y-values first, then the X-values.
    Coords = lists:map
        (fun(Y) -> lists:map(fun(X) -> {X, Y} end, Xs) end, Ys),
    RawCoordData = lists:zip(lists:flatten(Coords), Data),
    CoordData = lists:filter
        (fun({_, Index}) -> Index =/= TransparentIndex end, RawCoordData),

    SurfaceRef = sdl_video:getVideoSurface(),
    Surface = sdl_video:getSurface(SurfaceRef),

    lists:foreach(fun(Pixel) ->
        sdl_put_pixel(Surface, Pixel, ColorTable, Zoom) end, CoordData),
    sdl_video:updateRect(SurfaceRef, L * Zoom, T * Zoom, W * Zoom, H * Zoom),

    ok.

sdl_put_pixel(Surface, {{X, Y}, Index}, ColorTable, Zoom) ->
    DestRect = #sdl_rect{x=X * Zoom, y=Y * Zoom, w=Zoom, h=Zoom},
    #color{r=R, g=G, b=B} = lists:nth(Index + 1, ColorTable),
    SdlColor = sdl_video:mapRGB(Surface, R, G, B),

    sdl_video:fillRect(Surface, DestRect, SdlColor).

check_event() ->
    case sdl_events:pollEvent() of
        #quit{}                     -> quit;
        #keyboard{sym=?SDLK_q}      -> quit;
        #keyboard{sym=?SDLK_ESCAPE} -> quit;

        #keyboard{state=?SDL_RELEASED, sym=?SDLK_EQUALS} ->
            {zoom, ?ZOOM_INCREASE};
        #keyboard{state=?SDL_RELEASED, sym=?SDLK_MINUS} ->
            {zoom, ?ZOOM_DECREASE};

        no_event -> ok;

        #keyboard{state=?SDL_RELEASED, sym=$n}          -> next;
        #keyboard{state=?SDL_RELEASED, sym=?SDLK_SPACE} -> next;

        Event ->
            io:format("Got event ~p~n", [Event]),
            ok
    end.
