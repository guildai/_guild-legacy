%% Copyright 2016-2017 TensorHub, Inc.
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

-record(ps, {sec, secs, meta, lnum}).

load(File) ->
    case file:read_file(File) of
        {ok, Bin} -> parse(Bin);
        {error, Err} -> {error, Err}
    end.

parse(Bin) ->
    parse_lines(split_lines(Bin), init_parse_state()).

init_parse_state() ->
    #ps{sec=undefined, secs=[], meta=[], lnum=1}.

split_lines(Bin) ->
    re:split(Bin, "\r\n|\n|\r|\032", [{return, list}]).

parse_lines([Line|Rest], PS) ->
    parse_line(strip_leading_and_trailing_spaces(Line), Rest, PS);
parse_lines([], PS) ->
    {ok, finalize_parse(PS)}.

strip_leading_and_trailing_spaces(Str) ->
    string:strip(Str, both).

parse_line("", Rest, PS) ->
    parse_lines(Rest, incr_lnum(PS));
parse_line(";"++_, Rest, PS) ->
    parse_lines(Rest, incr_lnum(PS));
parse_line("#+"++Meta, Rest, PS) ->
    parse_lines(Rest, incr_lnum(add_meta(Meta, PS)));
parse_line("#"++_, Rest, PS) ->
    parse_lines(Rest, incr_lnum(PS));
parse_line("["++_=Line, Rest, PS) ->
    handle_section_parse(parse_section_line(Line, PS), Rest, PS);
parse_line(Line0, Rest0, PS0) ->
    case read_line_continuations(Line0, Rest0, PS0) of
        {ok, {Line, Rest, PS}} ->
            handle_attr_parse(parse_attr_line(Line, PS), Rest, PS);
        {error, Err} ->
            {error, Err}
    end.

add_meta("", PS) ->
    PS;
add_meta(Raw, #ps{meta=Meta}=PS) ->
    PS#ps{meta=[parse_meta(Raw)|Meta]}.

parse_meta(S) ->
    [meta_token(Part) || Part <- split_meta(S)].

split_meta(S) ->
    Pattern = "\"(.*)\"|([^\s]+)",
    Opts = [global, {capture, all_but_first, list}],
    {match, Parts} = re:run(S, Pattern, Opts),
    Parts.

meta_token(["", Word]) -> Word;
meta_token([Quoted]) -> Quoted.

parse_section_line(Line, #ps{lnum=Num}) ->
    Pattern =
        "^\\[\\s*([^ ]+)"
        "(?:\\s+\"(.+?)\")?"
        "(?:\\s+\"(.+?)\")?"
        "\\s*]$",
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, Keys} -> {ok, {Keys, []}};
        nomatch -> {error, {section_line, Num}}
    end.

handle_section_parse({ok, Section}, Rest, PS) ->
    parse_lines(Rest, incr_lnum(add_section(Section, PS)));
handle_section_parse({error, Err}, _Rest, _PS) ->
    {error, Err}.

add_section(New, #ps{sec=undefined}=PS) ->
    PS#ps{sec=New};
add_section(New, #ps{sec=Cur, secs=Secs}=PS) ->
    PS#ps{sec=New, secs=[finalize_section(Cur)|Secs]}.

finalize_section({Name, Attrs}) ->
    {Name, lists:reverse(Attrs)}.

read_line_continuations(Line, Rest, PS) ->
    {ok, Pattern} = re:compile("(.*?)\\\\$"),
    read_line_continuations_acc(Line, Rest, PS, Pattern, []).

read_line_continuations_acc(Line, Rest, PS, Pattern, Acc) ->
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, [Part]} ->
            handle_line_continuation(Part, Rest, PS, Pattern, Acc);
        nomatch ->
            finalize_line_continuation(Line, Acc, Rest, PS)
    end.

handle_line_continuation(Part, [NextLine|NextRest], PS, Pattern, Acc) ->
    read_line_continuations_acc(
      NextLine, NextRest, incr_lnum(PS), Pattern, [Part|Acc]);
handle_line_continuation(_Part, [], #ps{lnum=Num}, _Pattern, _Acc) ->
    {error, {eof, Num}}.

finalize_line_continuation(Line, Acc, Rest, PS) ->
    {ok, {lists:reverse([Line|Acc]), Rest, PS}}.

parse_attr_line(Line, #ps{lnum=Num}) ->
    Pattern = "([^\\s]+)\\s*[:=]\\s*(.*)",
    case re:run(Line, Pattern, [{capture, all_but_first, list}]) of
        {match, [Name, Val]} -> {ok, {Name, Val}};
        nomatch              -> {error, {attr_line, Line, Num}}
    end.

handle_attr_parse({ok, _}, _Rest, #ps{sec=undefined, lnum=Num}) ->
    {error, {no_section_for_attr, Num}};
handle_attr_parse({ok, Attr}, Rest, PS) ->
    parse_lines(Rest, incr_lnum(add_attr(Attr, PS)));
handle_attr_parse({error, Err}, _Rest, _PS) ->
    {error, Err}.

add_attr(Attr, #ps{sec={Name, Attrs}}=PS) ->
    PS#ps{sec={Name, [Attr|Attrs]}}.

finalize_parse(PS) ->
    apply_meta(finalize_sections(PS), PS).

finalize_sections(#ps{sec=undefined, secs=Acc}) ->
    lists:reverse(Acc);
finalize_sections(#ps{sec=Sec, secs=Acc}) ->
    lists:reverse([finalize_section(Sec)|Acc]).

apply_meta(Sections, #ps{meta=[]}) ->
    Sections;
apply_meta(Sections, #ps{meta=Meta}) ->
    [{'$meta', lists:reverse(Meta)}|Sections].

incr_lnum(#ps{lnum=N}=PS) -> PS#ps{lnum=N + 1}.
