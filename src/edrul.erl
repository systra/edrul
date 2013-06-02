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

-module(edrul).

-export([start/0, stop/0]).
%% rating API
-export([rate/1,
         reload_rates/0]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    edrul_app:start().

stop() ->
    edrul_app:stop().

%%%===================================================================
%%% Rating API
%%%===================================================================

rate(Params) when is_list(Params) ->
    edrul_jnode:rate(Params).

reload_rates() ->
    edrul_jnode:reload_rates().
