-module(aeplugin_rebar3).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, ae_plugin).
-define(DEPS, [compile]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = aeplugin_ae_resource:init(State),
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 ae_plugin"},
                                 {opts, []},
                                 {short_desc, "Generate ez archive of AE plugin."},
                                 {desc, desc()}
                                ]),
    {ok, rebar_state:add_provider(State1, Provider)}.

desc() ->
  "Generate Erlang code archive (http://erlang.org/doc/man/zip.html) of AE node plugin".

do(State) ->
    Providers = rebar_state:providers(State),
    Cwd = rebar_state:dir(State),
    rebar_hooks:run_project_and_app_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    Res = case rebar_state:project_apps(State) of
              [App] ->
                  rebar_api:info("Building archive...", []),
                  ExtDeps = ext_deps(App, State),
                  Files = files(App, ExtDeps, State),
                  archive(Files, State, App);
              _ ->
                  {error, {?MODULE, no_main_app}}
          end,
    rebar_hooks:run_project_and_app_hooks(Cwd, post, ?PROVIDER, Providers, State),
    Res.

archive(Files, State, App) ->
    Name = rebar_app_info:name(App),
    BaseDir = rebar_dir:base_dir(State),
    ArchiveName = filename:join(BaseDir, to_string([Name, ".ez"])),
    {ok, _} = zip:create(ArchiveName, Files),
    {ok, State}.

files(MainApp, Deps, State) ->
    lists:flatmap(
      fun(App) ->
              app_files(App)
      end, [MainApp | names_to_app_info(Deps, State)]).

app_files(App) ->
    [Name, Vsn, EbinDir, PrivDir] = [rebar_app_info:F(App) || F <- [name, vsn, ebin_dir, priv_dir]],
    Dir = to_string([Name, "-", Vsn]),
    Priv = files_in_dir(PrivDir),
    Ebin = files_in_dir(EbinDir),
    to_list(Dir, "ebin", Ebin)
        ++ to_list(Dir, "priv", Priv).

to_list(Dir, Type, Files) ->
  lists:filtermap(fun(File) ->
                      case file:read_file(File) of
                        {ok, Bin} ->
                          Split = lists:reverse(filename:split(File)),
                          TypeIndex = string:str(Split, [Type]),
                          Path = filename:join(lists:reverse(lists:sublist(Split, TypeIndex))),
                          {true, {filename:join([Dir, Path]), Bin}};
                        {error, Reason} ->
                          rebar_api:warn("Can't read file: ~p", [Reason]),
                          false
                      end
                  end, Files).

files_in_dir(Dir) ->
    lists:reverse(
      filelib:fold_files(Dir, ".*", true,
                         fun(F, Acc) -> [F|Acc] end, [])).

to_string(List) ->
  binary_to_list(iolist_to_binary(List)).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

ext_deps(App, State) ->
    case aeplugin_ae_resource:ae_deps(State) of
        undefined ->
            [];
        AeDeps ->
            Opts = rebar_app_info:opts(App),
            Deps = dict:fetch({deps,default}, Opts),
            ExtDeps = lists:foldr(
                        fun(A, Acc) ->
                                Name = element(1, A),
                                case lists:keymember(Name, 1, AeDeps)
                                    orelse lists:member(Name, Acc) of
                                    true ->
                                        Acc;
                                    false ->
                                        [Name | Acc]
                                end
                        end, [], Deps),
            rebar_api:info("ExtDeps = ~p", [ExtDeps]),
            ExtDeps
    end.

names_to_app_info(Apps, State) ->
    AppNames = [app_name(A) || A <- Apps],
    Deps = rebar_state:all_deps(State),
    [A || A <- Deps,
          lists:member(app_name(A), AppNames)].

app_name(A) when is_binary(A) ->
    A;
app_name(A) when element(1, A) == app_info_t ->
    rebar_app_info:name(A).
