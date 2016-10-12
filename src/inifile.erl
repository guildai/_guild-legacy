%% Copyright 2106 TensorHub, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(inifile).

-export([load/1, parse/1]).

-record(ps, {sec, secs, line}).

load(File) ->
    case file:read_file(File) of
        {ok, Bin} -> parse(Bin);
        {error, Err} -> {error, Err}
    end.

parse(Bin) ->
    parse_lines(split_lines(Bin), init_parse_state()).

init_parse_state() ->
    #ps{sec = undefined, secs = [], line = 1}.

split_lines(Bin) ->
    re:split(Bin, "\r\n|\n|\r|\032", [{return, list}]).

parse_lines([Line|Rest], PS) ->
    parse_line(strip_trailing_spaces(Line), Rest, PS);
parse_lines([], PS) ->
    {ok, finalize_parse(PS)}.

parse_line("", Rest, PS) ->
    parse_lines(Rest, incr_line(PS));
parse_line(";"++_, Rest, PS) ->
    parse_lines(Rest, incr_line(PS));
parse_line("#"++_, Rest, PS) ->
    parse_lines(Rest, incr_line(PS));
parse_line("["++_=Line, Rest, PS) ->
    handle_new_section(parse_section_line(Line, PS), Rest, PS);
parse_line(Line, _Rest, #ps{sec=undefined, line=Num}) ->
    {error, {no_section_for_attr, Line, Num}};
parse_line(Line, Rest, PS) ->
    handle_attr(parse_attr(Line, PS), Rest, PS).

parse_section_line(Line, #ps{line=Num}) ->
    Pattern = "\\[\\s*([^ ]+)(?:\\s+\"([^\"]*)\")?\\s*\\]",
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, Keys} -> {ok, {Keys, []}};
        nomatch       -> {error, {section_line, Line, Num}}
    end.

handle_new_section({ok, Section}, Rest, PS) ->
    parse_lines(Rest, incr_line(add_section(Section, PS)));
handle_new_section({error, Err}, _Rest, _PS) ->
    {error, Err}.

add_section(New, #ps{sec=undefined}=PS) ->
    PS#ps{sec=New};
add_section(New, #ps{sec=Cur, secs=Secs}=PS) ->
    PS#ps{sec=New, secs=[finalize_section(Cur)|Secs]}.

finalize_section({Name, Attrs}) ->
    {Name, lists:reverse(Attrs)}.

parse_attr(Line, #ps{line=Num}) ->
    Pattern = "([^\\s]+)\\s*[:=]\\s*(.*)",
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, [Name, Val]} -> {ok, {Name, Val}};
        nomatch -> {error, {attr_line, Line, Num}}
    end.

handle_attr({ok, Attr}, Rest, PS) ->
    parse_lines(Rest, add_attr(Attr, PS));
handle_attr({error, Err}, _Rest, _PS) ->
    {error, Err}.

add_attr(Attr, #ps{sec={Name, Attrs}}=PS) ->
    PS#ps{sec={Name, [Attr|Attrs]}}.

finalize_parse(#ps{sec=undefined, secs=Acc}) ->
    lists:reverse(Acc);
finalize_parse(#ps{sec=Sec, secs=Acc}) ->
    lists:reverse([finalize_section(Sec)|Acc]).

strip_trailing_spaces(Str) -> string:strip(Str, right).

incr_line(#ps{line=N}=PS) -> PS#ps{line=N + 1}.
