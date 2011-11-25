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

% TODO: License

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
    ParsedColors;

color_table(BinData, NumColorsLeft, ParsedColors) ->
    <<R:8, G:8, B:8, Rest/binary>> = BinData,
    NewParsedColors = [#color{r=R, g=G, b=B}|ParsedColors],

    color_table(Rest, NumColorsLeft - 1, NewParsedColors).

% The local color table, described later, is identical in structure to the
% global color table, and so can use the same parser.
global_color_table(BinData, NumColors) ->
    color_table(BinData, NumColors, []).

% == PARSED RECORD STRUCTURE ==================================================

% TODO: more data
-record(parsed_gif, {w, h, color_depth, colors=[]}).

% == MAIN ROUTINES ============================================================

go() ->
    case file:read_file("gfx/rgb-stripes.gif") of
        {ok, Data}      -> parse_data(Data);
        {error, Reason} ->
            io:format("Unable to open file: ~s~n", [Reason]),
            error
    end.

parse_data(<<Header:6/bytes, Rest/binary>>) ->
    io:format("~p~n", [<<Header:6/bytes, Rest/binary>>]),

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
    io:format("~p~n", [ParsedDataWH]),

    case global_color_table_flag(Lsd) of
        true  ->
            GCTableSize = global_color_table_size(Lsd),
            BGIndex     = background_color_index (Lsd),

            parse_global_color_table(Rest, ParsedDataWH,
                {{gc_table_size, GCTableSize},
                 {bg_index,      BGIndex    }});
        false ->
            parse_graphics_control_extension(Rest, ParsedDataWH)
    end.

parse_global_color_table(BinData, ParsedData,
    {{gc_table_size, GCTableSize},
     {bg_index     , BGIndex    }}) ->
    GCTableByteLen = GCTableSize * 3,
    <<BinGCTable:GCTableByteLen/bytes, Rest/binary>> = BinData,

    GCTable = global_color_table(BinGCTable, GCTableSize),
    ParsedDataGC = ParsedData#parsed_gif{colors=GCTable},

    % TODO: preserve BGIndex

    io:format("~p~n", [ParsedDataGC]),
    parse_graphics_control_extension(Rest, ParsedDataGC).

parse_graphics_control_extension(BinData, ParsedData) ->
    % TODO
    ok.

init_sdl(ParsedData) ->
    case init_video(ParsedData) of
        error ->
            sdl:quit(),
            error;
        ok    ->
            %draw_image(ParsedData),
            sdl:quit(),
            ok
    end.

init_video({{w, W}, {h, H}, _}) ->
    sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_ERLDRIVER),

    Bpp = 16,
    Surface = sd_video:setVideoMode(W, H, Bpp, ?SDL_SWSURFACE),

    case Surface of
        error ->
            io:format("Can't set video mode~n"),
            error;
        _     -> ok
    end.
