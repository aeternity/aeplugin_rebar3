-module(aeplugin_ae_resource).

-behaviour(rebar_resource_v2).

-export([init/1]).

%% Callbacks
-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

-export([ae_deps/1]).

init(State) ->
    rebar_api:info("At init: AllDeps = ~p", [rebar_state:all_deps(State)]),
    {ok, rebar_state:add_resource(State, {ae, ?MODULE})}.

init(Type, State) ->
    AeRoot = get_ae_root(State),
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{aeternity_root => AeRoot}),
    {ok, Resource}.

lock(AppInfo, State) ->
    lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo), State).

lock_(_AppDir, Lock, _State) when element(1,Lock) == ae ->
    Lock.

needs_update(_AppInfo, _State) ->
    %% TODO: Figure out how this should work
    false.

download(TmpDir, AppInfo, State, LState) ->
    rebar_resource_v2:download(TmpDir, ae_dep_app_info(AppInfo, LState, State), State).

make_vsn(AppInfo, LState) ->
    rebar_resource_v2:make_vsn(ae_dep_app_info(AppInfo, LState, undefined), unknown).

get_ae_root(State) ->
    CacheKey = {?MODULE, ae_root},
    case persistent_term:get(CacheKey, undefined) of
        undefined ->
            Res = case file:read_link("aeternity") of
                      {ok, AeRoot} ->
                          {local, AeRoot};
                      {error, _} ->
                          rebar_state:get(
                            State,
                            aeternity_root,
                            {git, "https://github.com/aeternity/aeternity", "master"})
                  end,
            rebar_api:info("Aeternity Root: ~p", [Res]),
            persistent_term:put(CacheKey, Res),
            Res;
        Cached ->
            Cached
    end.

ae_dep_app_info(AppInfo, LState, State) ->
    RealSource = get_ae_dep(AppInfo, LState, State),
    rebar_app_info:source(AppInfo, RealSource).

get_ae_dep(AppInfo, LState, State) ->
    Name = rebar_app_info:name(AppInfo),
    AeDeps = get_ae_rebar_lock(LState, State),
    case lists:keyfind(Name, 1, AeDeps) of
        {_, Dep, _} ->
            Dep;
        false ->
            error({cannot_find_ae_dep, Name})
    end.

get_ae_rebar_lock(#{aeternity_root := AE}, State) ->
    CacheKey = {?MODULE, ae_rebar_lock},
    case persistent_term:get(CacheKey, undefined) of
        undefined ->
            Res = case read_rebar_lock(AE) of
                      {ok, [{_, Apps}|_]} ->
                          Apps;
                      _ ->
                          []
                  end,
            persistent_term:put(CacheKey, Res),
            save_ae_deps(Res, State),
            Res;
        Lock ->
            Lock
    end.

read_rebar_lock({local, Dir}) ->
    LockF = filename:join(Dir, "rebar.lock"),
    file:consult(LockF);
read_rebar_lock(Source) ->
    fetch_rebar_lock(Source).

fetch_rebar_lock({git, URI, Ref}) ->
    RefBin = iolist_to_binary([Ref]),
    Res = rebar_httpc_adapter:request(
            get,
            <<"https://api.github.com/repos/aeternity/aeternity/contents/rebar.lock?ref=", RefBin/binary>>,
            #{ <<"Accept">> => <<"application/vnd.github.raw">>
             , <<"user-agent">> => user_agent()}, undefined, #{profile => rebar}),
    case Res of
        {ok, {200, _, Body}} ->
            rebar_api:info("Reading AE rebar.lock from ~p (~p)", [URI, Ref]),
            case erl_scan:string(binary_to_list(Body)) of
                {ok, Tokens, _} ->
                    {ok, parse_tokens_as_terms(Tokens)};
                _Error ->
                    error(parse_error)
            end;
        Other ->
            rebar_api:info("UNKNOWN response from github: ~p", [Other]),
            error(fetch_error)
    end.

save_ae_deps(Deps, State) ->
    case ae_deps_file(State) of
        undefined ->
            rebar_api:warn("Cannot save Aeternity deps", []),
            skip;
        File ->
            save_term(Deps, File)
    end.

ae_deps(State) ->
    case file:consult(ae_deps_file(State)) of
        {ok, [Deps]} ->
            Deps;
        Error ->
            rebar_api:warn("Cannot read ae_deps file: ~p", [Error]),
            undefined
    end.

ae_deps_file(State) ->
    BaseDir = rebar_dir:base_dir(State),
    filename:join(BaseDir, "aeternity_deps.eterm").

save_term(Term, File) ->
    rebar_api:info("Saving AE deps to ~p", [File]),
    {ok, Fd} = file:open(File, [write]),
    try io:fwrite(Fd, "~p.~n", [Term])
    after
        file:close(Fd)
    end.

user_agent() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    <<"(rebar3/", (list_to_binary(Vsn))/binary, ") (httpc)">>.

parse_tokens_as_terms(Tokens) ->
    [[]|Split] = lists:foldl(fun toksplit/2, [[]], Tokens),
    Sublists = lists:reverse(Split),
    [parse_term(Ts) || Ts <- Sublists].

toksplit({dot,_} = Dot, [H|T]) ->
    [[] | [lists:reverse([Dot|H]) | T]];
toksplit(Tok, [H|T]) ->
    [[Tok|H] | T].

parse_term(Tokens) ->
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.
