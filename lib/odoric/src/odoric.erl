%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2012 by Tristan Sloughter <>
%%%-------------------------------------------------------------------
-module(odoric).

%% API
-export([main/1]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
main(Args) ->
    ok = start_epmd(),

    %% Extract the args
    {ok, {Options, _RestArgs}} = getopt:parse(opt_spec_list(), Args),
    TargetNode = case lists:keyfind('sname', 1, Options) of
                     false ->
                         none;
                     undefined ->
                         TN = proplists:get_value('name', Options),
                         ThisNode = append_node_suffix(TN, "_maint_"),
                         {ok, _} = net_kernel:start([ThisNode, longnames]),
                         nodename(TN);
                     {sname, TN} ->
                         io:format("NAME ~p~n", [TN]),
                         ThisNode = append_node_suffix(TN, "_maint_"),
                         {ok, _} = net_kernel:start([ThisNode, shortnames]),
                         nodename(TN)
                 end,

    case lists:keyfind(cookie, 1, Options) of
        {cookie, Cookie} ->
            erlang:set_cookie(node(), Cookie);
        false ->
            nothing
    end,

    case TargetNode of
        none ->
            do_nothing;
        _ ->
            %% See if the node is currently running  -- if it's not, we'll bail    
            case {net_kernel:hidden_connect_node(TargetNode), net_adm:ping(TargetNode)} of
                {true, pong} ->
                    ok;
                {_, pang} ->
                    io:format("Node ~p not responding to pings.\n", [TargetNode]),
                    halt(1)
            end
    end,

    case lists:keyfind(action, 1, Options) of
        {action, "deploy"} ->
            start_erlcloud(),
            {url, Url} = lists:keyfind(url, 1, Options),
            Artifacts = odoric_builder:build(Url),
            odoric_uploader:upload(s3, Artifacts),
            ok;
        {action, "ping"} ->
            %% If we got this far, the node already responsed to a ping, so just dump
            %% a "pong"
            io:format("pong\n");
        {action, "stop"} ->
            io:format("~p\n", [rpc:call(TargetNode, init, stop, [], 60000)]);
        {action, "restart"} ->
            io:format("~p\n", [rpc:call(TargetNode, init, restart, [], 60000)]);
        {action, "reboot"} ->
            io:format("~p\n", [rpc:call(TargetNode, init, reboot, [], 60000)]);
        {action, "rpc"} ->
            {module, Module} = lists:keyfind(module, 1, Options),
            {function, Function} = lists:keyfind(function, 1, Options),
            {args, RpcArgs} = lists:keyfind(args, 1, Options),
            case rpc:call(TargetNode, list_to_atom(Module), list_to_atom(Function),
                          [RpcArgs], 60000) of
                ok ->
                    ok;
                {badrpc, Reason} ->
                    io:format("RPC to ~p failed: ~p\n", [TargetNode, Reason]),
                    halt(1);
                _ ->
                    halt(1)
            end;
        {action, "rpcterms"} ->
            {module, Module} = lists:keyfind(module, 1, Options),
            {function, Function} = lists:keyfind(function, 1, Options),
            {args, ArgsAsString} = lists:keyfind(args, 1, Options),
            case rpc:call(TargetNode, list_to_atom(Module), list_to_atom(Function),
                          consult(ArgsAsString), 60000) of
                {badrpc, Reason} ->
                    io:format("RPC to ~p failed: ~p\n", [TargetNode, Reason]),
                    halt(1);
                Other ->
                    io:format("~p\n", [Other])
            end
    end,
    net_kernel:stop().

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_erlcloud() ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(xmerl),
    ok = application:start(erlcloud).

start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

epmd_path() ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    Name = "epmd",
    case os:find_executable(Name, ErtsBinDir) of
        false ->
            case os:find_executable(Name) of
                false ->
                    io:format("Could not find epmd.~n"),
                    halt(1);
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.

nodename(Name) ->
    case string:tokens(Name, "@") of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = string:tokens(atom_to_list(node()), "@"),
            list_to_atom(lists:concat([Node, "@", Host]))
    end.

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, "@") of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.

%%
%% Given a string or binary, parse it into a list of terms, ala file:consult/0
%%
consult(Str) when is_list(Str) ->
    consult([], Str, []);
consult(Bin) when is_binary(Bin)->
    consult([], binary_to_list(Bin), []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
                {ok, Tokens, _} ->
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    consult([], Remaining, [Term | Acc]);
                {eof, _Other} ->
                    lists:reverse(Acc);
                {error, Info, _} ->
                    {error, Info}
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.

opt_spec_list() ->
    [
     {cookie,      $c, "set-cookie", atom,   "Cookie"},
     {name,        $s, "name",       string, "Short node name"},
     {sname,       $s, "sname",      string, "Long node name"},
     {url,        $u, "url",       string, "Url of git repo"},
     {action,      undefined, undefined,     string, "Action"},
     {module,      undefined, undefined,     string, "odule to call function of"},
     {function,      undefined, undefined,     string, "Function to call"},
     {args,      undefined, undefined,     string, "Args to pass function"}
    ].
