%%%-------------------------------------------------------------------
%%% @author Tristan Sloughter <>%%% @copyright (C) 2012, Tristan Sloughter
%%% @doc
%%%
%%% @end
%%% Created : 19 April 2012 by Tristan Sloughter <tristan@mashape.com>
%%%-------------------------------------------------------------------
-module(odoric_git).

%% API
-export([clone/2]).

clone(Url, Dir) ->
    io:format("Cloning ~s to ~s...~n", [Url, Dir]),
    Cmd = io_lib:format("git clone ~s ~s", [Url, Dir]),
    os:cmd(Cmd).
