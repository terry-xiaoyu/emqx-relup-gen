-module(emqx_relup_gen_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, relup_gen).
-define(DEPS, [{default, release}]).
-define(CLI_OPTS, [
    {relup_dir, $d, "relup-dir", {string, "./relup"},
        "The directory that contains upgrade path file and the *.relup files."
        " The upgrade path file defaults to 'upgrade_path.list'."},
    {path_file_name, $u, "path-file-name", {string, "upgrade_path.list"},
        "The filename that describes the upgrade path."}
]).
-define(INCLUDE_DIRS, ["bin", "lib", "plugins", "releases"]).
-define(TAR_FILE(VSN), VSN ++ ".tar.gz").

-ifdef(TEST).
-export([gen_compelte_relup/3]).
-endif.

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, emqx},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "./rebar3 emqx relup_gen --relup-dir=./relup"},
            {profiles, ['emqx-enterprise']},
            {opts, ?CLI_OPTS},
            {short_desc, "A rebar plugin that helps to generate relup tarball for emqx."},
            {desc, "A rebar plugin that helps to generate relup tarball for emqx."
                   " The tarball will contain a *.relup file and all the necessary"
                   " files to upgrade the emqx release."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try safe_do(State)
    catch
        throw:Reason ->
            {error, {?MODULE, Reason}}
    end.

safe_do(State) ->
    {RawArgs, _} = rebar_state:command_parsed_args(State),
    RelupDir = getopt_relup_dir(RawArgs),
    TargetVsn = get_release_vsn(State),
    ErtsVsn = get_erts_vsn(),
    OtpVsn = get_otp_vsn(),
    TarFile = ?TAR_FILE(TargetVsn),
    PathFile = getopt_upgrade_path_file(RelupDir, RawArgs),
    rebar_log:log(info, "generating relup tarball for: ~p", [TargetVsn]),
    rebar_log:log(debug, "using relup dir: ~p", [RelupDir]),
    rebar_log:log(debug, "using upgrade path_file: ~p", [PathFile]),
    #{target_vsn := TargetVsn, upgrade_path := UpgradePath}
        = get_upgrade_path(PathFile, TargetVsn),
    Relups = load_partial_relup_files(RelupDir),
    CompleteRelup = gen_compelte_relup(Relups, TargetVsn, UpgradePath),
    ok = save_relup_file(CompleteRelup, TargetVsn, State),
    ok = make_relup_tarball(TarFile, ErtsVsn, OtpVsn, State),
    rebar_log:log(info, "relup tarball generated: ~p", [TarFile]),
    {ok, State}.

get_upgrade_path(PathFile, TargetVsn) ->
    case file:script(PathFile) of
        {ok, PathDescList} ->
            PathDescList1 = lists:map(fun parse_path_desc/1, PathDescList),
            Search = fun(#{target_vsn := Vsn}) -> Vsn =:= TargetVsn end,
            case lists:search(Search, PathDescList1) of
                false ->
                    Reason = #{target_vsn => TargetVsn, file => PathFile},
                    throw({target_vsn_not_found_in_path_file, Reason});
                {value, PathDesc} ->
                    PathDesc
            end;
        {error, Reason} ->
            throw({get_upgrade_path_error, #{error => Reason, file => PathFile}})
    end.

parse_path_desc(FullPathStr) ->
    case parse_upgrade_path_str(FullPathStr) of
        [_] -> throw({invalid_upgrade_path, #{path_str => FullPathStr}});
        [TargetVsn | UpgradePath] ->
            #{target_vsn => TargetVsn, upgrade_path => UpgradePath}
    end.
parse_upgrade_path_str(FullPathStr) ->
    [string:trim(S) || S <- string:split(FullPathStr, "<-", all), S =/= ""].

load_partial_relup_files(RelupDir) ->
    %% read all of the *.relup files in the relup directory
    case filelib:wildcard(filename:join([RelupDir, "*.relup"])) of
        [] ->
            throw({no_relup_files_found, #{dir => RelupDir}});
        Files ->
            lists:foldl(fun(File, Acc) ->
                case file:script(File) of
                    {ok, #{target_version := TargetVsn, from_version := FromVsn} = Relup} ->
                        ok = validate_relup_file(Relup),
                        Acc#{{TargetVsn, FromVsn} => Relup};
                    {error, Reason} ->
                        throw({load_partial_relup_files_error,
                                #{error => Reason, file => File}})
                end
            end, #{}, Files)
    end.

save_relup_file(Relup, TargetVsn, State) ->
    Filename = io_lib:format("~s.relup", [TargetVsn]),
    RelupFile = filename:join([get_rel_dir(State), "releases", TargetVsn, Filename]),
    case file:write_file(RelupFile, io_lib:format("~p.", [Relup])) of
        ok -> ok;
        {error, Reason} ->
            throw({save_relup_file_error, #{error => Reason, file => RelupFile}})
    end.

%% Assume that we have a UpgradePath = 5 <- 3 <- 2 <- 1, then we need to have
%%  following relup files: (2 <- 1).relup, (3 <- 2).relup, (5 <- 3).relup, then we
%%  can generate a complete relup file that includes all direct paths:
%%  5 <- 3, 5 <- 2, 5 <- 1
gen_compelte_relup(Relups, TargetVsn, UpgradePath0) ->
    [FirstFromVsn | UpgradePath] = UpgradePath0,
    TargetRelups0 = case search_relup(TargetVsn, FirstFromVsn, Relups) of
        error -> throw({relup_not_found, #{from => FirstFromVsn, target => TargetVsn}});
        {ok, Relup} -> [Relup]
    end,
    {_, TargetRelups, _} = lists:foldl(fun(FromVsn, {LastResolvedFromVsn, TargetRelups1, RelupsAcc}) ->
            case search_relup(TargetVsn, FromVsn, RelupsAcc) of
                error ->
                    {ok, Part1} = search_relup(TargetVsn, LastResolvedFromVsn, RelupsAcc),
                    case search_relup(LastResolvedFromVsn, FromVsn, RelupsAcc) of
                        error ->
                            throw({relup_not_found, #{from => FromVsn, target => LastResolvedFromVsn}});
                        {ok, Part2} ->
                            Relup0 = concat_relup(Part1, Part2),
                            {FromVsn, [Relup0 | TargetRelups1],
                             RelupsAcc#{{TargetVsn, FromVsn} => Relup0}}
                    end;
                {ok, Relup1} ->
                    {FromVsn, [Relup1 | TargetRelups1], RelupsAcc}
            end
        end, {FirstFromVsn, TargetRelups0, Relups}, UpgradePath),
    lists:reverse(TargetRelups).

search_relup(TargetVsn, FromVsn, Relups) ->
    maps:find({TargetVsn, FromVsn}, Relups).

concat_relup(#{target_version := A, from_version := B} = Relup1,
             #{target_version := B, from_version := C} = Relup2) ->
    CodeChanges = maps:get(code_changes, Relup2, []) ++ maps:get(code_changes, Relup1, []),
    Callbacks = maps:get(post_upgrade_callbacks, Relup2, []) ++ maps:get(post_upgrade_callbacks, Relup1, []),
    #{
        target_version => A,
        from_version => C,
        code_changes => normalize_code_changes(CodeChanges),
        post_upgrade_callbacks => Callbacks
    };
concat_relup(#{target_version := A, from_version := B},
             #{target_version := C, from_version := D}) ->
    throw({cannot_concat_relup, #{relup1 => {A, B}, relup2 => {C, D}}}).

make_relup_tarball(TarFile, ErtsVsn, _OtpVsn, State) ->
    RelDir = get_rel_dir(State),
    Files = lists:map(fun(Dir) ->
        FullPathDir = filename:join([RelDir, Dir]),
        {Dir, FullPathDir}
    end, [lists:concat(["erts-", ErtsVsn]) | ?INCLUDE_DIRS]),
    ok = r3_hex_erl_tar:create(TarFile, Files, [compressed]).

%-------------------------------------------------------------------------------
get_rel_dir(State) ->
    filename:join([rebar_dir:base_dir(State), "rel", "emqx"]).

get_otp_vsn() ->
    VsnFile = filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]),
    {ok, Version} = file:read_file(VsnFile),
    binary_to_list(Version).

get_erts_vsn() ->
    VsnFile = filename:join([code:root_dir(), "releases", "start_erl.data"]),
    {ok, Content} = file:read_file(VsnFile),
    case string:split(Content, " ") of
        [ErtsVsn, _OtpVsn] -> binary_to_list(string:trim(ErtsVsn));
        _ -> throw({parse_start_erl_data_failed, VsnFile})
    end.

get_release_vsn(State) ->
    RelxOpts = rebar_state:get(State, relx, []),
    case lists:keyfind(release, 1, rebar_state:get(State, relx, [])) of
        false ->
            throw({relx_opts_not_found, #{relx_opts => RelxOpts}});
        {release, {_Name, RelxVsn}, _} ->
            RelxVsn
    end.

getopt_relup_dir(RawArgs) ->
    case proplists:get_value(relup_dir, RawArgs) of
        undefined -> "relup";
        RelupDir -> RelupDir
    end.

getopt_upgrade_path_file(RelupDir, RawArgs) ->
    case proplists:get_value(path_file_name, RawArgs) of
        undefined -> throw({missing_cmd_opts, path_file_name});
        PathFile -> filename:join([RelupDir, PathFile])
    end.

%-------------------------------------------------------------------------------
validate_relup_file(_Relup) ->
    ok.

normalize_code_changes(Instrucitons) ->
    %% 1. remove duplicated instructions
    %% 2. sort according to dependent relationship
    Instrucitons.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
