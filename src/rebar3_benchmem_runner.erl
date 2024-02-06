-module(rebar3_benchmem_runner).

-export([run/3]).

run(Mod, Fun, Opts) ->
    process_flag(trap_exit, true),
    ok = do_run(Mod, Fun, Opts, []), % warmup once to ensure all code is loaded
    Flags =
        case maps:get(set_on_spawn, Opts, true) of
            true  -> [call, silent, set_on_spawn];
            false -> [call, silent]
        end,
    {_, TracedMods} = enable_trace(),
    ok = do_run(Mod, Fun, Opts, Flags), % FIXME: decide what to do with errors
    Stat = collect(TracedMods),
    ok = disable_trace(TracedMods),
    Stat.

do_run(Mod, Fun, _Opts, Flags) ->
    Pid = proc_lib:spawn_link(
        fun() ->
            with_setup(
                fun(St) ->
                    Input = input(Mod, Fun, St),
                    Flags == [] orelse erlang:trace(self(), true, Flags),
                    Mod:Fun(Input, St),
                    Flags == [] orelse erlang:trace(self(), false, Flags)
                end,
                Mod,
                Fun
            )
        end
    ),
    receive
        {'EXIT', Pid, normal} ->
            ok;
        {'EXIT', Pid, Reason} ->
            {error, Reason}
    end.

collect(Pattern) ->
    maps:fold(fun collect_trace/3, [], Pattern).

collect_trace(Mod, FunList, Acc) ->
    {Fail, Ret} = lists:foldl(
        fun ({Fun, Arity}, {Fail, Prev}) ->
            case combine_trace(erlang:trace_info({Mod, Fun, Arity}, call_memory)) of
                skip ->
                    {Fail, Prev};
                fail ->
                    {true, Prev};
                Tr ->
                    {Fail, [{Mod, Fun, Arity, Tr} | Prev]}
            end
        end, {false, Acc}, FunList),
    %% module may have been hot-code reloaded, or tracing was broken by something else
    Fail andalso begin
        rebar_api:info(
            "encountered an error tracing module ~s, was it reloaded or untraced?",
            [Mod])
        end,
    Ret.

combine_trace({call_memory, []}) ->
    skip;
%% It is possible that due to hot code reload event
%% some function is no longer traced, while it was supposed to.
%% Reinstating tracing automatically is wrong thing to do, because
%% statistics won't be correct anyway. Hence the warning in the user
%% guide, guarding against hot code reload while tracing.
combine_trace({call_memory, false}) ->
    fail;
combine_trace({call_memory, Mem}) ->
    case [{Pid, Calls, Words} || {Pid, Calls, Words} <- Mem, Words > 0] of
        [] ->
            skip;
        NonZero ->
            NonZero
    end.

enable_trace() ->
    lists:foldl(
        fun({Mod, _}, {Total, Acc}) ->
            Plus = erlang:trace_pattern({Mod, '_', '_'}, true, [call_memory]),
            {Total + Plus, Acc#{Mod => Mod:module_info(functions)}}
        end,
        {0, #{}},
        code:all_loaded()
    ).

disable_trace(Map) ->
    maps:foreach(
        fun(Mod, _Funs) ->
            erlang:trace_pattern({Mod, '_', '_'}, false, [call_memory])
        end,
        Map
    ).

with_setup(F, Mod, Fun) ->
    St = opts_call(Mod, Fun, init, []),
    try F(St)
    after
        opts_call(Mod, Fun, {stop, St}, [])
    end.

input(Mod, Fun, St) ->
    opts_call(Mod, Fun, {input, St}, []).

opts_call(Mod, Name, Arg, Default) ->
    F = opts_fun(Mod, Name),
    try F(Arg)
    catch error:R when R == undef;
                       R == function_clause ->
            Default
    end.

opts_fun(Mod, Fun) ->
    "bench_" ++ NameS = atom_to_list(Fun),
    Name = list_to_atom(NameS),
    fun Mod:Name/1.
