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

-module(guild_depot_http).

-export([start_server/2, stop_server/0]).

-export([handle_page/2]).

-export([init/1, handle_msg/3]).

%% ===================================================================
%% Start / stop server
%% ===================================================================

start_server(Port, Opts) ->
    guild_dtl:init(Opts),
    App = create_app(Opts),
    ServerOpts = [{proc_callback, {?MODULE, []}}],
    guild_http_sup:start_server(Port, App, ServerOpts).

init([]) ->
    erlang:register(?MODULE, self()).

stop_server() ->
    proc:cast(?MODULE, stop).

handle_msg(stop, _From, _App) ->
    {stop, normal}.

%% ===================================================================
%% Main app / routes
%% ===================================================================

create_app(Opts) ->
    psycho_util:chain_apps(routes(), middleware(Opts)).

routes() ->
    psycho_route:create_app(
      [{{starts_with, "/assets/"}, static_handler()},
       {"/bye",                    bye_handler()},
       {'_',                       page_handler()}
      ]).

static_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

bye_handler() ->
    fun(_Env) -> handle_bye() end.

handle_bye() ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    guild_http:ok_text("Stopping server\n").

page_handler() ->
    psycho_util:dispatch_app(
      {?MODULE, handle_page},
      [method, parsed_path]).

middleware(Opts) ->
    maybe_apply_log_middleware(Opts, []).

maybe_apply_log_middleware(Opts, Acc) ->
    case proplists:get_bool(log, Opts) of
        true -> [log_middleware()|Acc];
        false -> Acc
    end.

log_middleware() ->
    fun(Upstream) -> guild_log_http:create_app(Upstream) end.

%% ===================================================================
%% Page handler
%% ===================================================================

handle_page("GET", {"/", _, Params}) ->
    handle_index(Params);
handle_page(_, _) ->
    guild_http:bad_request().

handle_index(_Params) ->
    Vars =
        [{html_title, "Depot - Guild"},
         {nav_title, "Guild Depot"},
         {projects, fake_projects()},
         {filter_tags, fake_filter_tags()}],
    Page = guild_dtl:render(guild_depot_index_page, Vars),
    guild_http:ok_html(Page).

fake_projects() ->
    [
     [{id, "8e25642a-63f6-4bed-b650-ca507589ce70"},
      {path, "gar1t/mnist"},
      {description, "Models for MNIST digit recognition"},
      {name, "MNIST"},
      {account, "gar1t"},
      {updated, "Updated 3 minutes ago"},
      {tags, [tag("MNIST"), tag("CNN")]},
      {stars, 12}],
     [{id, "ce7243ad-51c2-49c3-bb2c-2e37e68d4420"},
      {path, "jjallaire/cifar10"},
      {description, "Models for CIFAR-10 image recognition"},
      {name, "CIFAR 10"},
      {account, "jjallaire"},
      {updated, "Updated on Dec 10, 2016"},
      {tags, [tag("CIFAR-10"), tag("CNN")]},
      {stars, 1}],
     [{id, "060a1801-0f60-4547-94c5-3ade09c8f26f"},
      {path, "jjallaire/mnist"},
      {description, "Models for MNIST digit recognition"},
      {name, "MNIST"},
      {account, "jjallaire"},
      {updated, "Updated 6 days ago"},
      {tags, [tag("MNIST")]},
      {stars, 5}]
    ].

tag(Name) ->
    [{name, Name}, {color, color_for_name(Name)}].

color_for_name(Name) ->
    case (erlang:phash2(Name, 3) + 1) of
        0 -> "#888";
        1 -> "rgb(82,135,198)";
        2 -> "rgb(48,163,141)";
        3 -> "rgb(197,182,102)"
    end.

fake_filter_tags() ->
    [tag("Dataset", 36),
     tag("TensorFlow", 29),
     tag("Model", 27),
     tag("Resource", 25),
     tag("Paper", 23),
     tag("Images", 18),
     tag("Deep Learning", 12),
     tag("CNN", 10),
     tag("Music", 4),
     tag("ImageNet", 3),
     tag("AlexNet", 3),
     tag("LSTM", 3),
     tag("MLP", 2),
     tag("Video", 2),
     tag("Speech", 2),
     tag("Torch", 2),
     tag("Reinforcement", 2),
     tag("Classifier", 2),
     tag("Tutorial", 2),
     tag("Inception", 2),
     tag("Keras", 2),
     tag("Word2vec", 1),
     tag("DCGAN", 1),
     tag("Motion", 1),
     tag("Titanic", 1),
     tag("Scikit Flow", 1),
     tag("Scikit-learn", 1),
     tag("Software", 1),
     tag("Unsupervised", 1),
     tag("Multi GPU", 1),
     tag("Bayesian", 1),
     tag("Ratings", 1),
     tag("nlp", 1),
     tag("chatbot", 1),
     tag("seq2seq", 1),
     tag("Website", 1),
     tag("Tracking", 1),
     tag("Art", 1),
     tag("GRU", 1),
     tag("MR", 1),
     tag("Julia", 1),
     tag("Book", 1),
     tag("Spark", 1),
     tag("Supervised", 1),
     tag("Kaggle", 1)].

tag(Name, Count) ->
    [{name, Name}, {count, Count}].
