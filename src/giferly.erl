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
-export([go/0]).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").

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

% The graphics control extension may appear only once, if at all, before a
% graphic rendering block, but we won't bother checking that.
read_all_extension_blocks(BinData, Extensions) ->
    case BinData of
        <<16#21, _/binary>> ->
            case extension_block(BinData) of
                {graphics_control, ParsedGCE, NewBinData} ->
                    read_all_extension_blocks
                        (NewBinData, [ParsedGCE|Extensions]);
                 NewBinData                               ->
                    read_all_extension_blocks(NewBinData, Extensions)
            end;
        _                   ->
            {Extensions, BinData}
    end.

% --- Graphics Control Extension ----------------------------------------------

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

% The next byte in the graphics control extension, when not "0", specifies the
% amount of time to wait before displaying the next image. The value is given
% in 1/100ths of a second and is counted starting immediately after the image
% is rendered.

% The next byte is the transparency index. This is relevant only if the
% transparency index is set in the packed byte mentioned before. If so, any
% pixels with this index is unmodified when rendering.

% Technically, the block has a size field, and a block terminator, but in this
% case, the block has a constant size, with a fixed structure.
extension_block(<<16#21f904:24,
    Packed:8/bits, DelayTime:16/little, TransparentIndex:8,
    0:8, Rest/binary>>) ->
    DisposalMethod = disposal_method(Packed),
    ParsedTransparencyIndex = case transparency_flag(Packed) of
        true  -> TransparentIndex;
        false -> -1
    end,

    ParsedGCE = {graphics_control_extension,
                    {disposal    , DisposalMethod         },
                    {delay       , DelayTime              },
                    {transparency, ParsedTransparencyIndex}},

    {graphics_control, ParsedGCE, Rest};

% --- Comment Extension -------------------------------------------------------

extension_block(<<16#21fe:16, BinData/binary>>) ->
    <<Size:8, CommentRest/binary>> = BinData,
    <<Comment:Size/bytes, 0, Rest/binary>> = CommentRest,
    io:format("Comment: ~s~n", [Comment]),

    Rest.

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
lzw_minimum_code_size(<<Size:8, _/binary>>) ->
    Size.

% The rest of the block is grouped into data sub-blocks. Each sub-block starts
% with a single byte stating the number of bytes of data in the following
% sub-block, followed by that many bytes of data.
image_data_all_sub_blocks(BinData) ->
    image_data_next_sub_block(BinData, <<>>).

image_data_next_sub_block(<<   0:8, Rest/binary>>, SubBlocks) ->
    {SubBlocks, Rest};
image_data_next_sub_block(<<Size:8, Rest/binary>>, SubBlocks) ->
    <<Block:Size/bytes, RemainingSubBlocks/binary>> = Rest,
    SubBlocksNew = <<SubBlocks/binary, Block/binary>>,
    image_data_next_sub_block(RemainingSubBlocks, SubBlocksNew).

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

% ---- Data Stream Representation ---------------------------------------------

% The LZW compression algorithm converts a stream of indices into a stream of
% codes, with each code corresponding to some pattern of consecutive indices.
% The codes themselves are not stored as a single byte per code, as that would
% limit the number of codes to 256, as well as taking up unnecessary space for
% codes when there are fewer unique codes in the table at that point during the
% execution of the compression algorithm. Thus, the number of bits used to
% store a code varies over the execution of the algorithm.

% Because a single code may be smaller than one byte and may span more than one
% byte (say taking a few bits from one byte and the remaining bits from the
% following byte). For example, given the bytes:
%
%   01100100 00101010
%
% and a code size of 5 bytes, the first code would be the least significant
% bits of the first byte, i.e. 00100 = 4. This leaves the pattern:
%
%    011     00101010
%
% so the next code is 10 011, where "011" comes from the first byte and "10"
% comes from the second. This introduces a number of issues:
%
%  * We can't just match a bitstring, because the actual values to be extracted
%    need not be at the very beginning of the bitstring.
%  * We can't reverse the entire bitstring, because individual bytes need to
%    preserved. We also can't just reverse the bitstring byte by byte, because
%    then the value to be extracted ends up at the end of the bitstring.
%  * We need a way to record how many bits have already been extracted from the
%    first byte, so we would have to pass that information around.
%
% Instead of dealing with these problems via a bitstring, we can convert the
% entire bitstring into a list of 8-character strings consisting of "1"s and
% "0"s, and we can perform string manipulation on this list. Obviously, this is
% memory and time-intensive, but it's a solution that can be reasoned about. In
% the future, it may be wise to develop a more abstract data structure in which
% we can store the state, but for learning the language, a simpler solution
% will more than suffice.

lzw_binary_to_string_list(<<>>, List) ->
    lists:reverse(List);
lzw_binary_to_string_list(<<Byte:8, Rest/binary>>, List) ->
    NList = [string:right(erlang:integer_to_list(Byte, 2), 8, $0)|List],
    lzw_binary_to_string_list(Rest, NList).

lzw_next_code([Byte|CDataRest], CodeSoFar, CSizeLeft) ->
    ByteSize = length(Byte),

    if
        ByteSize >= CSizeLeft ->
            NCodeSoFar = string:substr
                (Byte, ByteSize - CSizeLeft + 1, CSizeLeft) ++ CodeSoFar,
            NCSizeLeft = 0,

            NByte = string:substr(Byte, 1, ByteSize - CSizeLeft),
            case NByte of
                [] -> NCData = CDataRest;
                _  -> NCData = [NByte|CDataRest]
            end;
        true ->
            NCData = CDataRest,
            NCodeSoFar = CodeSoFar ++ Byte,
            NCSizeLeft = CSizeLeft - ByteSize
    end,

    if NCSizeLeft =:= 0 -> {NCData, erlang:list_to_integer(NCodeSoFar, 2)};
       true             -> lzw_next_code(NCData, NCodeSoFar, NCSizeLeft)
    end.

% ---- Decompression Algorithm ------------------------------------------------

% The exact decompression algorithm has the following characteristics:
%
%  * Originally, the code size is the LZW Minimum Code Size plus one, since
%    the Minimum Code Size is only sufficient for a table consisting only of
%    the colors, and not the two special codes, CLEAR and EOI.
%  * Whenever the CLEAR code is encountered, the code table is rebuilt, and
%    the current code size goes back to the original code size. The next code
%    is immediately retrieved, and its value in the table is sent to the output
%    stream. This is because the maximum code size is 12 bits. An encoder may
%    choose to say with this code size when it is reached, not adding any more
%    patterns to the code table.
%  * After the first code following the CLEAR code is decoded, the following
%    loop is executed:
%
%    1. Let CODE be the next code in the code stream, and {CODE} its
%       corresponding pattern in the code table, if the code is in the table.
%       Similarly, CODE-1 is the code that has just been decoded, and {CODE-1}
%       its corresponding pattern.
%    2. If CODE is in the table:
%       a. Output {CODE} to the output stream.
%       b. Let K be the first index in {CODE}.
%       c. Add {CODE-1} ++ [K] to the code table.
%    3. IF CODE is not in the table:
%       a. Let K be the first index in {CODE-1}.
%       b. Output {CODE-1} ++ [K] to the output stream.
%       c. Add {CODE-1} ++ [K] to the code table.
%    4. Restart loop.
%
%  * When the code table contains the maximum number of codes addressable by
%    the current code size (i.e. after the code with index 2^{CodeSize}-1 is
%    added), the code size is increased by 1.
%  * When the EOI code is encountered, decompression is complete, and the loop
%    is terminated.

lzw_decode_single(CData, DData, PrevCode, CSize, Table, NumColors, MinCSize) ->
    {NCData, CCode} = lzw_next_code(CData, "", CSize),

    case lzw_code_table_lookup(Table, CCode) of
        {ok, clear} ->
            NTable = lzw_code_table_new(NumColors),
            {NCData2, NPrevCode} = lzw_next_code(NCData, "", MinCSize),
            {ok, Pattern} = lzw_code_table_lookup(Table, NPrevCode),
            NDData = DData ++ Pattern,

            lzw_decode_single(NCData2,
                NDData, NPrevCode, MinCSize, NTable, NumColors, MinCSize);
        {ok, eoi  } ->
            DData;
        _           ->
            {ok, PrevPattern} = lzw_code_table_lookup(Table, PrevCode),
            case lzw_code_table_lookup(Table, CCode) of
                {ok, Pattern} ->
                    NDData = DData ++ Pattern,
                    K = hd(Pattern),

                    NPattern = PrevPattern ++ [K],
                    NTable = lzw_code_table_add(Table, NPattern);
                error       ->
                    K = hd(PrevPattern),
                    NPattern = PrevPattern ++ [K],
                    NDData = DData ++ NPattern,
                    NTable = lzw_code_table_add(Table, NPattern)
            end,

            NCSize = case lzw_code_table_full(NTable, CSize) of
                true  -> CSize + 1;
                false -> CSize
             end,

            case NCData of
                [] -> DData;
                _  -> lzw_decode_single(NCData,
                    NDData, CCode, NCSize, NTable, NumColors, MinCSize)
            end
    end.

lzw_decode(ImageData, LZWMinCodeSize, NumColors) ->
    CData = lzw_binary_to_string_list(ImageData, []),
    Table = lzw_code_table_new(NumColors),
    CSize = LZWMinCodeSize + 1,

    lzw_decode_single(CData, [], undefined, CSize, Table, NumColors, CSize).

% == TRAILER ==================================================================

% The final byte of the file constitutes the trailer block, which indicates
% that no more data follows. The block is always the byte "3b".
end_of_file(<<16#3b>>) -> true ;
end_of_file(_        ) -> false.

% == PARSED RECORD STRUCTURE ==================================================

-record(parsed_gif, {w, h, color_depth, disposal, delay, transparency,
    colors=[], images=[]}).
-record(parsed_img, {l, t, w, h, colors=[], data=[]}).

% == MAIN ROUTINES ============================================================

go() ->
    case file:read_file("gfx/rgb-stripes-transparent.gif") of
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
            io:format("Invalid header: ~P~n", [Header]),
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
            parse_extension_blocks(Rest, ParsedDataWH)
    end.

parse_global_color_table(BinData, ParsedData,
    {{gc_table_size, GCTableSize},
     {bg_index     , BGIndex    }}) ->
    GCTableByteLen = GCTableSize * 3,
    <<BinGCTable:GCTableByteLen/bytes, Rest/binary>> = BinData,

    GCTable = global_color_table(BinGCTable, GCTableSize),
    ParsedDataGC = ParsedData#parsed_gif{colors=GCTable},

    % TODO: preserve BGIndex

    parse_extension_blocks(Rest, ParsedDataGC).

parse_extension_blocks(BinData, ParsedData) ->
    {Extensions, Rest} = read_all_extension_blocks(BinData, []),
    NewParsedData =
        case lists:keyfind(graphics_control_extension, 1, Extensions) of
            {graphics_control_extension,
                {disposal    , DisposalMethod   },
                {delay       , DelayTime        },
                {transparency, TransparencyIndex}} ->
                ParsedData#parsed_gif{disposal=DisposalMethod,
                    delay=DelayTime, transparency=TransparencyIndex};
            false                                  ->
                ParsedData
        end,

    parse_image_descriptor(Rest, NewParsedData).

parse_image_descriptor(BinData, ParsedData) ->
    Eof = end_of_file(BinData),
    if Eof ->
        io:format("Parsed image: ~p~n", [ParsedData]),
        io:format("Finished parsing image...~n"),
        ParsedData;
       true ->
        <<ImDesc:10/bytes, Rest/binary>> = BinData,
        {{l, L}, {t, T}, {w, W}, {h, H}} = image_descriptor_dim(ImDesc),
        ParsedImage = #parsed_img{l=L, t=T, w=W, h=H},

        case local_color_table_flag(ImDesc) of
            true  ->
                LCTableSize = local_color_table_size(ImDesc),
                BGIndex     = background_color_index(ImDesc),

                parse_local_color_table(Rest, ParsedData, ParsedImage,
                    {{lc_table_size, LCTableSize},
                     {bg_index,      BGIndex    }});
            false ->
                parse_image_data(Rest, ParsedData, ParsedImage)
        end
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
    <<LZWMinCodeSize:8, SubBlocks/binary>> = BinData,
    NumColors = length(ParsedData#parsed_gif.colors),

    {ParsedImageData, Rest} = image_data_all_sub_blocks(SubBlocks),
    ImageDataDecoded = lzw_decode(ParsedImageData, LZWMinCodeSize, NumColors),

    OldImages = ParsedData#parsed_gif.images,
    ParsedImageFull = ParsedImage#parsed_img{data=ImageDataDecoded},
    ParsedDataIm = ParsedData#parsed_gif{images=[ParsedImageFull|OldImages]},

    parse_image_descriptor(Rest, ParsedDataIm).

% == MAIN SDL ROUTINES ========================================================

init_sdl(#parsed_gif{images=[]}) ->
    io:format("No images to display; shutting down...~n"),
    ok;
init_sdl(ParsedData) ->

    case init_video(ParsedData) of
        error ->
            sdl:quit(),
            error;
        ok    ->
            draw_background(),
            draw_image(ParsedData, []),
            io:format("Shutting down...~n"),
            sdl:quit(),
            ok
    end.

init_video(#parsed_gif{w=W, h=H}) ->
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),

    Bpp = 16,
    Surface = sdl_video:setVideoMode(W, H, Bpp, ?SDL_SWSURFACE),
    % TODO: set the title to the image filename?
    sdl_video:wm_setCaption("giferly", []),

    case Surface of
        error ->
            io:format("Can't set video mode~n"),
            error;
        _     -> ok
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


draw_image(ParsedData, []) ->
    #parsed_gif{images=Images} = ParsedData,
    draw_image(ParsedData, Images);
draw_image(ParsedData, [Image|ImagesRest]) ->
    % TODO: check if local color table is present and use it if it is
    ColorTable = ParsedData#parsed_gif.colors,
    paint_pixels(ParsedData, Image, ColorTable),

    case check_event() of
        ok ->
            timer:sleep(100),
            draw_image(ParsedData, [Image|ImagesRest]);
        next ->
            timer:sleep(100),
            io:format("Moving to next image...~n"),
            draw_image(ParsedData, ImagesRest);
        quit ->
            ok
    end.

paint_pixels(#parsed_gif{transparency=TransparentIndex},
    #parsed_img{l=L, t=T, w=W, h=H, data=Data}, ColorTable) ->
    Xs = lists:seq(L, L + W - 1),
    Ys = lists:seq(T, T + H - 1),
    Coords = lists:map
        (fun(A) -> lists:map(fun(B) -> {A, B} end, Ys) end, Xs),
    RawCoordData = lists:zip(lists:flatten(Coords), Data),
    CoordData = lists:filter
        (fun({_, Index}) -> Index =/= TransparentIndex end, RawCoordData),

    SurfaceRef = sdl_video:getVideoSurface(),
    Surface = sdl_video:getSurface(SurfaceRef),

    lists:foreach(fun(Pixel) -> sdl_put_pixel(Surface, Pixel, ColorTable) end,
        CoordData),
    sdl_video:updateRect(SurfaceRef, L, T, W, H),

    ok.

sdl_put_pixel(Surface, {{X, Y}, Index}, ColorTable) ->
    DestRect = #sdl_rect{x=X, y=Y, w=1, h=1},
    #color{r=R, g=G, b=B} = lists:nth(Index + 1, ColorTable),
    SdlColor = sdl_video:mapRGB(Surface, R, G, B),

    sdl_video:fillRect(Surface, DestRect, SdlColor).

check_event() ->
    case sdl_events:pollEvent() of
        #quit{}                     -> quit;
        #keyboard{sym=?SDLK_q}      -> quit;
        #keyboard{sym=?SDLK_ESCAPE} -> quit;

        no_event -> ok;

        #keyboard{state=?SDL_RELEASED, sym=$n}          -> next;
        #keyboard{state=?SDL_RELEASED, sym=?SDLK_SPACE} -> next;

        Event ->
            io:format("Got event ~p~n", [Event]),
            ok
    end.
