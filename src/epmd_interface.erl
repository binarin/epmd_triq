-define(DD(__Fmt, __Args),
        begin
            __UserMsg = io_lib:format(__Fmt, __Args),
            {{__Year, __Month, __Day}, {__Hour, __Minute, __Second}} = calendar:now_to_local_time(erlang:timestamp()),
            __Msg = io_lib:format("[~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B ~s ~s:~B] ~s~n",
                                  [__Year, __Month, __Day, __Hour, __Minute, __Second, node(), ?MODULE, ?LINE, __UserMsg]),
            file:write_file("/tmp/erl-debug.log", __Msg, [append]),
            error_logger:info_msg("~s", [__Msg])
        end).

-module(epmd_interface).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([names/0, start_fake/1]).

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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec names() -> [binary()].
names() ->
    gen_server:call(?SERVER, names).

-spec start_fake(atom()) -> ok.
start_fake(NodeName) ->
    gen_server:call(?SERVER, {start_fake, NodeName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    EpmdPort = open_port({spawn_executable, "/usr/bin/epmd"},
                         [{args, ["-port", "6666"]}, exit_status]), 
    PseudoBinary = code:priv_dir(epmd_triq) ++ "/pseudo_node",
    ?DD("~p", [PseudoBinary]),
    {ok, #state{epmd_port = EpmdPort
               ,pseudo_nodes = #{}
               ,pseudo_binary = PseudoBinary}}.

handle_call(names, _From, State) ->
    NamesBlob = os:cmd("epmd -port 6666 -names"),
    Names = case re:run(NamesBlob, "^name (.+?) at port ([0-9]+)", [multiline, global, {capture, all_but_first, list}]) of
                {match, RawNamesAndPorts} ->
                    [list_to_atom(Name) || [Name, _Port] <- RawNamesAndPorts];
                _ ->
                    []
            end, 
    {reply, Names, State};
handle_call({start_fake, NodeName}, _From, #state{pseudo_nodes = PseudoNodes} = State) ->
    FakePort = open_port({spawn_executable, State#state.pseudo_binary}, 
                         [exit_status, use_stdio, {args, ["--name", atom_to_binary(NodeName, utf8)]}]),
    NewPseudoNodes = PseudoNodes#{NodeName => FakePort}, 
    Reply = ok, 
    {reply, Reply, State#state{pseudo_nodes = NewPseudoNodes}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
