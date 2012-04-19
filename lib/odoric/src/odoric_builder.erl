%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 19 April 2012 by Tristan Sloughter <tristan@mashape.com>
%%%-------------------------------------------------------------------
-module(odoric_builder).

%% API
-export([build/1]).

build(Url) ->
    {ok, Cwd} = file:get_cwd(),
    Dir = os:cmd("mktemp -d"),
    %% remove \n
    TmpDir = lists:sublist(Dir, 1, length(Dir)-1),

    odoric_git:clone(Url, TmpDir),    
    file:set_cwd(TmpDir),
    
    io:format("Building with sinan...~n"),
    os:cmd("sinan dist"),

    file:set_cwd(Cwd),
    
    {ok, Artifacts} = file:list_dir(filename:join(TmpDir, "_build/tar")),
    [filename:join([TmpDir, "_build/tar", Artifact]) || Artifact <- Artifacts].

    

