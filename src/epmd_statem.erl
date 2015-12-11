-module(epmd_statem).
-include_lib("triq/include/triq.hrl").
-include_lib("triq/include/triq_statem.hrl").
-compile([export_all]).

-define(NODES, [a, b, c, d]).

-record(state, {started :: list(atom())
               ,not_started :: list(atom())
               }).

prop_epmd_statem() ->
    ?FORALL(Cmds,
            commands(?MODULE),
            begin
                catch(epmd_interface:stop()),
                {ok, _} = epmd_interface:start_link(),
                run_commands(?MODULE, Cmds),
                ok == epmd_interface:stop()
            end).

initial_state() ->
    #state{started = [], not_started = ?NODES}.

command(#state{not_started = [], started = [_|_] = StopCandidates}) ->
    {call, epmd_interface, stop_fake, [oneof(StopCandidates)]};
command(#state{not_started = [_|_] = StartCandidates, started = []}) ->
    {call, epmd_interface, start_fake, [oneof(StartCandidates)]};
command(#state{not_started = [_|_] = Candidates, started = [_|_] = StopCandidates}) ->
    oneof([{call, epmd_interface, start_fake, [oneof(Candidates)]}
          ,{call, epmd_interface, stop_fake, [oneof(StopCandidates)]}
          ]).

precondition(_, _) ->
    true.

postcondition(_, _, _) ->
    true.

next_state(#state{started = Started, not_started = NotStarted} = State, _Var, {call, epmd_interface, start_fake, [Node]}) ->
    State#state{not_started = lists:delete(Node, NotStarted)
               ,started = [Node] ++ lists:delete(Node, Started)};
next_state(State, _, _) ->
    State.
