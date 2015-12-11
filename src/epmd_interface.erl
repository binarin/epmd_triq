-module(epmd_interface).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([names/0, start_fake/1, stop_fake/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {epmd_port
               ,pseudo_nodes
               ,pseudo_binary
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec names() -> [binary()].
names() ->
    gen_server:call(?SERVER, names).

-spec start_fake(atom()) -> ok.
start_fake(NodeName) ->
    gen_server:call(?SERVER, {start_fake, NodeName}).

-spec stop_fake(atom()) -> ok | not_running.
stop_fake(NodeName) ->
    gen_server:call(?SERVER, {stop_fake, NodeName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{epmd_port = start_epmd_daemon()
               ,pseudo_nodes = []
               ,pseudo_binary = find_pseudo_node_binary()}}.

handle_call(names, _From, State) ->
    {reply, get_names_from_running_epmd(), State};
handle_call({start_fake, NodeName}, _From, State) ->
    StartResult = start_fake_node_process(NodeName, State#state.pseudo_binary),
    handle_start_fake_node_result(StartResult, NodeName, State);
handle_call({stop_fake, NodeName}, _From, State) ->
    handle_stop_fake_call(lists:keyfind(NodeName, 1, State#state.pseudo_nodes), State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_start_fake_node_result({ok, _Port}, _NodeName, State) ->
    {reply, ok, State};
handle_start_fake_node_result(rejected, _NodeName, State) ->
    {reply, ok, State}.

start_fake_node_process(NodeName, ExecBinary) ->
    Port = open_port({spawn_executable, ExecBinary},
                     [exit_status, use_stdio, stream, binary,
                      {args, ["--port", "6666", "--name", atom_to_binary(NodeName, utf8)]}]),
    receive
        {Port, {data, <<"ok">>}} ->
            {ok, Port};
        {Port, {data, <<"rejected">>}} ->
            rejected;
        {Port, {exit_status, _}} ->
            early_exit
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({EpmdPort, {exit_status, _}}, #state{epmd_port = EpmdPort} = State) ->
    {stop, epmd_lost, State};
handle_info({Port, {exit_status, ExitStatus}}, State) ->
    {noreply, handle_port_exit_status(Port, ExitStatus, State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_epmd_daemon() ->
    Port = open_port({spawn_executable, find_epmd_proxy_binary()},
                     [{args, ["-port", "6666"]}, exit_status, use_stdio]),
    timer:sleep(50),
    Port.

find_epmd_proxy_binary() ->
    code:priv_dir(epmd_triq) ++ "/epmd_proxy".

find_pseudo_node_binary() ->
    code:priv_dir(epmd_triq) ++ "/pseudo_node".

get_names_from_running_epmd() ->
    NamesBlob = os:cmd("epmd -port 6666 -names"),
    Names = case re:run(NamesBlob, "^name (.+?) at port ([0-9]+)", [multiline, global, {capture, all_but_first, list}]) of
                {match, RawNamesAndPorts} ->
                    [list_to_atom(Name) || [Name, _Port] <- RawNamesAndPorts];
                _ ->
                    []
            end,
    Names.

handle_port_exit_status(Port, _ExitStatus, #state{pseudo_nodes = Nodes} = State) ->
    case lists:keymember(Port, 2, Nodes) of
        true ->
            State#state{pseudo_nodes = lists:keydelete(Port, 2, Nodes)};
        false ->
            State
    end.

handle_stop_fake_call(false, State) ->
    {reply, not_running, State};
handle_stop_fake_call({NodeName, Port}, State) ->
    Port ! {self(), close},
    NewNodes = lists:keydelete(NodeName, 1, State#state.pseudo_nodes),
    {reply, ok, State#state{pseudo_nodes = NewNodes}}.
