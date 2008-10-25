%%% File    : erlbal_http_be.erl
%%% Author  : Todd Lipcon <todd@lipcon.org>
%%% Description : 
%%% Created : 23 Oct 2008 by Todd Lipcon <todd@lipcon.org>

-module(erlbal_http_be).


-export([handle_request/2]).


handle_request(HostPort, MochiReq)
  when element(1, MochiReq) =:= mochiweb_request,
       is_list(HostPort) ->
    Method = list_to_atom(string:to_lower(atom_to_list(MochiReq:get(method)))),
    BEUrl = "http://" ++ HostPort ++ MochiReq:get(path),
    io:format("BE Url: ~p~n", [BEUrl]),
    do_be_req(Method, BEUrl, MochiReq);

handle_request({url, URL}, MochiReq) when element(1, MochiReq) =:= mochiweb_request ->
    do_be_req(get, URL, MochiReq).
    

do_be_req(Method, BEUrl, MochiReq) ->
    ReqHdrs = mochiweb_headers:to_list(MochiReq:get(headers)),
    ReqHdrsLists = [{any_to_list(K), any_to_list(V)} ||
                    {K, V} <- ReqHdrs],

    Req = {BEUrl, ReqHdrsLists},
    {ok, ReqId} = http:request(
                    Method, Req,
                    [{autoredirect, false}],
                    [{stream, {self, once}},
                     {sync, false}]),

    io:format("ReqId is: ~p~n", [ReqId]),
    receive
        {http, {ReqId, stream_start, RespHeaders, StreamPid}} ->
            handle_stream_start(MochiReq, ReqId, StreamPid, RespHeaders);

        {http, {ReqId, {StatusLine = {"HTTP" ++ _HTTPVersion, StatusCode, Explanation},
                        Headers,
                        Body}}} when is_list(Headers), is_binary(Body) ->
            error_logger:info_msg("Got immediate response!"),
            MochiReq:respond({StatusCode, Headers, Body}),
            {ok, done}                        
    after 20000 ->
            error_logger:error_msg("20sec timeout on request to backend ~p~n", [BEUrl]),
            {error, timeout}
    end.

handle_stream_start(MReq, ReqId, StreamPid, RespHeaders) ->
    error_logger:info_msg("Response headers: ~p~n", [RespHeaders]),
    case proplists:get_value("x-reproxy-url", RespHeaders) of
        undefined ->
            MResp = MReq:start_response({200,
                                         RespHeaders}),
            receive_loop(MReq, MResp, ReqId, StreamPid);
        Reproxy ->
            error_logger:info_msg("Reproxy: ~p~n", [Reproxy]),

            % We no longer need the original Backend request
            http:cancel_request(ReqId),
            
            % However we might have something in our queue for this backend
            % so flush any messages
            flush_http_req(ReqId),
            {ok, {reproxy, Reproxy}}
    end.

receive_loop(MReq, MResp, ReqId, StreamPid) ->
    http:stream_next(StreamPid),
    receive
        {http, {ReqId, stream, Binary}} ->
            MResp:send(Binary),
            receive_loop(MReq, MResp, ReqId, StreamPid);
        {http, {ReqId, stream_end, _Headers}} ->
            {ok, done};
        {http, Other} ->
            exit({bad_http_message, Other})
    after 10000 ->
            {error, timeout}
    end.


flush_http_req(ReqId) ->
    receive
        {http, {ReqId, _, _, _}} ->
            flush_http_req(ReqId);
        {http, {ReqId, _, _}} ->
            flush_http_req(ReqId);
        {http, {ReqId, _}} ->
            flush_http_req(ReqId)
    after 0 ->
            ok
    end.
         
any_to_list(X) when is_list(X) -> X;
any_to_list(X) when is_atom(X) -> atom_to_list(X);
any_to_list(X) when is_binary(X) -> binary_to_list(X).
