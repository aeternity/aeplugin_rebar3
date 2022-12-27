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
    Vsn = rebar_app_info:vsn(App),
    Name = rebar_app_info:name(App),
    ArchiveName = to_string([Name, "-", Vsn, ".ez"]),

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
    Priv = filelib:wildcard(filename:join(PrivDir, "**/*"), file),
    Ebin = filelib:wildcard(filename:join(EbinDir, "*.{beam,app}"), file),
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

to_string(List) ->
  binary_to_list(iolist_to_binary(List)).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ext_deps() ->
%%     {ok, Bin} = file:read_file("ext_deps.txt"),
%%     re:split(Bin, "\\W", [{return,binary}]).

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
                                case lists:keymember(Name, 1, AeDeps) of
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

state_to_map(State) ->
    if_escript(),
    Fields = record_fields(state_t, rebar_state),
    [_|Attrs] = tuple_to_list(State),
    maps:from_list(lists:zip(Fields, Attrs)).

record_fields(state_t, rebar_state) ->
    [dir,opts,code_paths,default,escript_path,lock,
     current_profiles,namespace,command_args,command_parsed_args,
     current_app,project_apps,deps_to_build,all_plugin_deps,
     all_deps,compilers,project_builders,resources,providers,
     allow_provider_overrides];
record_fields(Rec, Mod) ->
    {ok, {_,[{abstract_code,{_,Forms}}]}} =
        beam_lib:chunks(code:which(Mod), [abstract_code]),
    [Defs] = [D || {attribute,_,record,{Name,D}} <- Forms,
                   Name == Rec],
    lists:map(
      fun(E) when element(1,E) == record_field ->
              {atom,_,F} = element(3,E),
              F;
         (E) when element(1,E) == typed_record_field ->
              RfE = element(2,E),
              {atom,_,F} = element(3,RfE),
              F
      end, Defs).
               
if_escript() ->
    case escript:script_name() of
        File when is_list(File) ->
            {ok, Sections} = escript:extract(File, []),
            {_, Archive} = lists:keyfind(archive,1,Sections),
            %% Dir = zip:list_dir(Archive),
            %% Extract = zip:foldl(
            %%             fun("rebar/ebin/rebar_state.beam",_,GetBin, Acc) ->
            %%                     {ok, GetBin()};
            %%                (_, _, _, Acc) ->
            %%                     Acc
            %%             end, error, Archive),
            {ok,[{_, Bin}]} = zip:extract(Archive, [{file_list, ["rebar/ebin/rebar_state.beam"]},memory]),
            Bin;
        Other ->
            {ok, Bin} = file:read_file(code:which(rebar_state)),
            Bin
    end.
