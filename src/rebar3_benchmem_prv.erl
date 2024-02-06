-module(rebar3_benchmem_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, benchmem).
-define(DEPS, [compile, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {profiles, [test]},
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 benchmem"}, % How to use the plugin
        {opts, opts()},               % list of options understood by the plugin
        {short_desc, "Run memory consumption benchmarks"},
        {desc, "Run memory consumption benchmarks.~n"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    Benches = find_benches(State, CliOpts),
    Baseline = [], % TODO: compare reports
    run_benches(Benches, Baseline, CliOpts),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format(?MODULE_STRING ":~w benchmem error: ~p", [?LINE, Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

opts() ->
    [
        {dir, $d, "dir", {string, "test"},
         "directory where the benchmark tests are located"},
        {module, $m, "module", string,
         "name of one or more modules to run (comma-separated)"},
        {benches, $b, "bench", string,
         "name of benchmark to run within a specified module (comma-separated)"},
        {raw, $r, "raw", {boolean, false},
         "output raw statistics"}
    ].

run_benches(Benches, _Baseline, OptsL) ->
    Opts0 = maps:from_list(OptsL),
    Opts = Opts0#{log_fun => fun rebar_api:debug/2},
    lists:map(
        fun({Mod, Fun}) ->
            rebar_api:info("Testing ~s:~s()", [Mod, Fun]),
            Stats = rebar3_benchmem_runner:run(Mod, Fun, Opts),
            case maps:get(raw, Opts, false) of
                true  -> rebar_api:info("Raw data:~n~p~n", [Stats]);
                false -> ok
            end,
            Total = total(Stats),
            rebar_api:info("Total words allocated: ~w~n", [Total]),
            {{Mod, Fun}, Total}
      end,
      Benches
    ).

total(Stats) ->
    lists:foldl(
        fun({_Mod, _Fun, _Arity, Allocs}, Total) ->
            lists:foldl(
                fun({_Pid, _Calls, Words}, SubTotal) ->
                    SubTotal + Words
                end,
                Total,
                Allocs
            )
        end,
        0,
        Stats
    ).

find_benches(State, Opts) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    Mods = maybe_parse_csv(proplists:get_value(module, Opts, any)),
    Benches = maybe_parse_csv(proplists:get_value(benches, Opts, any)),
    Found = find_benches(State, Dir, Mods, Benches),
    rebar_api:debug("Found: ~p", [Found]),
    {ModsFound0, BenchesFound0} = lists:unzip(Found),
    ModsFound = [atom_to_list(Mod) || Mod <- ModsFound0],
    BenchesFound = [atom_to_list(Bench) || Bench <- BenchesFound0],
    Benches =/= any andalso
        [throw({bench_not_found, Bench, Mods})
         || Bench <- Benches, not lists:member(Bench, BenchesFound)],
    Mods =/= any andalso
        [throw({module_not_found, Mod, Benches})
         || Mod <- Mods, not lists:member(Mod, ModsFound)],
    Found.

maybe_parse_csv(Atom) when is_atom(Atom) -> Atom;
maybe_parse_csv(Data) ->
    case is_atom_list(Data) of
        true -> [atom_to_list(D) || D <- Data];
        false -> parse_csv(Data)
    end.

is_atom_list([]) -> true;
is_atom_list([H|T]) when is_atom(H) -> is_atom_list(T);
is_atom_list(_) -> false.

parse_csv(IoData) ->
    re:split(IoData, ", *", [{return, list}]).

find_benches(State, Dir, Mods, Benches) ->
    rebar_api:debug("Dir: ~p", [Dir]),
    rebar_api:debug("Mods: ~p", [Mods]),
    rebar_api:debug("Benches: ~p", [Benches]),
    %% Fetch directories and app configs
    RawDirs = [{{rebar_app_info:name(App),
                 filename:join([rebar_app_info:out_dir(App), Dir])},
                filename:join(rebar_app_info:dir(App), Dir)}
               || App <- rebar_state:project_apps(State),
                  not rebar_app_info:is_checkout(App)],
    %% Pick a root test directory for umbrella apps
    UmbrellaDir =
        [{{<<"root">>,
           filename:join(rebar_dir:base_dir(State), "bench_"++Dir)},
         P} || P <- [make_absolute_path(filename:join([".", Dir]))],
               not lists:member(P, [D || {_,D} <- RawDirs])],
    TestDirs = RawDirs ++ UmbrellaDir,
    rebar_api:debug("SearchDirs: ~p", [TestDirs]),
    %% Keep directories with benches in them
    Dirs = [{App, TestDir}
            || {App, TestDir} <- TestDirs,
               {ok, Files} <- [file:list_dir(TestDir)],
               lists:any(fun(File) -> bench_suite(Mods, File) end, Files)],
    rebar_api:debug("Dirs: ~p", [Dirs]),
    [Bench || {_, TestDir} <- Dirs,
             {ok, Files} <- [file:list_dir(TestDir)],
             File <- Files,
             bench_suite(Mods, File),
             Bench <- benches(Benches, module(File))].

make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

bench_suite(Mods, File) ->
    Mod = filename:basename(File, ".erl"),
    filename:extension(File) =:= ".erl"
    andalso
    ((Mods =:= any andalso lists:prefix("bench_", Mod))
     orelse
     (Mods =/= any andalso lists:member(Mod, Mods))).

benches(any, Mod) ->
    [{Mod, Bench} || {Bench, 2} <- Mod:module_info(exports), bench_prefix(Bench)];
benches(Benches, Mod) ->
    [{Mod, Bench} || {Bench, 2} <- Mod:module_info(exports),
                    lists:member(atom_to_list(Bench), Benches)].

bench_prefix(Atom) ->
    lists:prefix("bench_", atom_to_list(Atom)).

module(File) ->
    list_to_atom(filename:basename(File, ".erl")).
