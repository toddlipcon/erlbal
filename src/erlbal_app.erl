%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlbal application.

-module(erlbal_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlbal.
start(_Type, _StartArgs) ->
    erlbal_deps:ensure(),
    erlbal_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlbal.
stop(_State) ->
    ok.
