%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for erlbal.

-module(erlbal_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {ok, DefaultBackend} = application:get_env(default_backend),
    Loop = fun (Req) ->
		   ?MODULE:loop(Req, DefaultBackend)
	   end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DefaultBE) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
	Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            handle_req_loop(DefaultBE, Req);
	'POST' ->
	    case Path of
		_ ->
		    Req:not_found()
	    end;
	_ ->
	    Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


handle_req_loop(BE, Req) ->
    case dispatch_to_backend(BE, Req) of
        {ok, {redirect, NewBE, NewReq}} ->
            handle_req_loop(NewBE, NewReq);

        {ok, {reproxy, RPHeader}} ->
            try_reproxy(string:tokens(RPHeader, " "),
                        Req);

        {ok, done} ->
            ok;
        
        Err = {error, _} -> Err
    end.

dispatch_to_backend({balancer, Name}, Req) when is_atom(Name) ->
    erlbal_balancer:handle_request(Name, Req);

dispatch_to_backend({http, HostPort}, Req) ->
    erlbal_http_be:handle_request(HostPort, Req).

try_reproxy([URL | Rest], Req) ->
    BE = {http, {url, URL}},
    case handle_req_loop(BE, Req) of
        Err = {error, _} ->
            error_logger:info_msg("Failed reproxy: ~p. Trying rest: ~p~n",
                                  [Err, Rest]),
            try_reproxy(Rest, Req);

        Else ->
            Else
    end;

try_reproxy([], _Req) ->
    {error, no_reproxies_left}.
