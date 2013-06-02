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

-module(edrul_jnode).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%% state transitions
-export([starting/2,
         starting/3,
         handshaking/2,
         handshaking/3,
         ready/2,
         ready/3,
         stopped/2,
         stopped/3]).

%% API
-export([start/0, stop/0, rate/1, reload_rates/0]).

-record(state, {peer :: any(),
                timer :: reference(),
                port :: port()}).

-define(SERVER, ?MODULE).
-define(PEERNAME, jnode).
-define(MSG(X, Y), {X, self(), make_ref(), Y}).
-define(MSG(X), ?MSG(X, undefined)).
-define(CALL_TIMEOUT, 5000).
-define(RECONNECT_INTERVAL, 10000).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_fsm:sync_send_all_state_event(?SERVER, start).

stop() ->
    gen_fsm:sync_send_all_state_event(?SERVER, stop).

-type rating_parameter() :: {Key :: string() | binary(), Value :: string() | binary() | integer() | float() | atom() | boolean()}.
-type rating_step() :: {rating_step, Priority::non_neg_integer(), BalanceName::binary(), Units::integer()}.
-type rating_result() :: {rating_result, Steps::[rating_step()], RatingGroup::binary()}.
-spec rate([rating_parameter()]) -> {ok, Result :: rating_result()} | {error, Reason :: term()}.
rate(Params) when is_list(Params) ->
    case call_peer(?MSG(rate, Params)) of
        {ok, {rating_result, Cascade, _}} when length(Cascade) =:= 0 -> {error, undefined_rating};
        Other -> Other
    end.

reload_rates() ->
    call_peer(?MSG(reload)).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []). %[{debug, [trace]}]).


%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, Hostname} = inet:gethostname(),
    Peer = {?PEERNAME, list_to_atom(atom_to_list(?PEERNAME) ++ "@" ++ Hostname)},
    State = #state{ peer = Peer },
    {ok, handshaking, handshake(State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handshaking(handshake_timeout, State) ->
    {ok, Port} = start_jnode(),
    {next_state, starting, State#state{port = Port}};
handshaking(_Event, State) ->
    {next_state, handshaking, State}.

starting(restart, State) ->
    {ok, Port} = start_jnode(),
    {next_state, starting, State#state{port = Port}};
starting(_Event, State) ->
    {next_state, starting, State}.

ready(_Event, State) ->
    {next_state, ready, State}.

stopped(_Event, State) ->
    {next_state, stopped, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handshaking(_Event, _From, State) ->
    {reply, ok, handshaking, State}.

starting(_Event, _From, State) ->
    {reply, ok, starting, State}.

ready(_Event, _From, State) ->
    {reply, ok, ready, State}.

stopped(_Event, _From, State) ->
    {reply, ok, stopped, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(start, _From, stopped, State) ->
    gen_fsm:send_event(self(), restart),
    {reply, ok, starting, State};
handle_sync_event(stop, _From, ready, State) ->
    ok = call_jnode(State#state.peer, ?MSG(stop)),
    {reply, ok, stopped, State};
handle_sync_event(get_peer, _From, ready, State) ->
    {reply, {ok, State#state.peer}, ready, State};
handle_sync_event(get_peer, _From, StateName, State) ->
    {reply, {error, no_answer}, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(eof, StateName, State) ->
    {next_state, StateName, State}; % ignore any eof from port
handle_info({_ , {data, _}}, StateName, State) ->
    {next_state, StateName, State}; % ignore any data from port (log when appropriate)
handle_info({Port, {exit_status, 0}}, starting, State = #state{port = Port}) ->
    timer:sleep(3000), % give time JVM to start completly
    {next_state, handshaking, handshake(State#state{port = undefined})};
handle_info({Port, {exit_status, Code}}, starting, State = #state{port = Port}) ->
    % port exited with error try reconnect after while
    error_logger:error_msg("failed starting jnode. starting script returned: ~p~n", [Code]),
    Timer = gen_fsm:send_event_after(?RECONNECT_INTERVAL, restart),
    {next_state, starting, State#state{timer = Timer, port = undefined}};
handle_info({reply, {ok, From}}, handshaking, State) ->
    % handshake reply received
    _ = gen_fsm:cancel_timer(State#state.timer),
    erlang:link(From),
    {next_state, ready, State#state{timer = undefined}};
handle_info({'EXIT', _, noconnection}, ready, State) ->
    error_logger:info_msg("jnode exited. restarting.~n"),
    gen_fsm:send_event(self(), restart),
    {next_state, starting, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
call_peer(Msg) ->
    case gen_fsm:sync_send_all_state_event(?SERVER, get_peer) of
        {ok, Peer} -> call_jnode(Peer, Msg);
        Error -> Error
    end.

call_jnode(Peer, Message) ->
    Peer ! Message,
    receive
        {reply, Reply} -> Reply;
        _Any -> {error, invalid_reply}
    after ?CALL_TIMEOUT ->
        {error, no_answer}
    end.

handshake(State) ->
    State#state.peer ! ?MSG(handshake),
    Timer = gen_fsm:send_event_after(?CALL_TIMEOUT, handshake_timeout),
    State#state{timer = Timer}.

start_jnode() ->
    JavaCmd = code:priv_dir(edrul) ++ "/java/bin/jnode",
    PortSettings = [{args, ["start"]},
                     exit_status,
                     stderr_to_stdout, {line, 1024}],
    Port = erlang:open_port({spawn_executable, JavaCmd}, PortSettings),
    {ok, Port}.
