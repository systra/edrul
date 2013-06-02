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

-module(edrul_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
        ?CHILD(edrul_jnode, worker)
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
