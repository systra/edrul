%%%-------------------------------------------------------------------
%%% @author Andrzej Trawinski <at@systra.com.pl>
%%% @copyright (C) 2013 Andrzej Trawinski
%%% @end
%%%
%%% LICENSE:
%%% This source file is subject to the new BSD license bundled with
%%% this package in the file, LICENSE. This license is also available
%%% through the web at:
%%% http://www.opensource.org/licenses/bsd-license.php.
%%%-------------------------------------------------------------------

-module(edrul_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% API
-export([start/0, stop/0, get_env/1, get_env/2, get_priv_dir/0]).

-define(APPNAME, edrul).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    edrul_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% API
%% ===================================================================

% For console start
start() ->
    start(?APPNAME).

start(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {not_started, Dependency}} ->
            start(Dependency),
            start(App)
    end.

stop() ->
    application:stop(?APPNAME).

get_env(Key) ->
    application:get_env(?APPNAME, Key).

get_env(Key, Default) ->
    case get_env(Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.

get_priv_dir() ->
    code:priv_dir(?APPNAME).
