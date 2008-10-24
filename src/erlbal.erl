%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erlbal).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the erlbal server.
start() ->
    erlbal_deps:ensure(),
    ensure_started(crypto),
    application:start(erlbal).

%% @spec stop() -> ok
%% @doc Stop the erlbal server.
stop() ->
    Res = application:stop(erlbal),
    application:stop(crypto),
    Res.
