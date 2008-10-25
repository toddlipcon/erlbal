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
    do_be_req(Method, BEUrl, MochiReq);

handle_request({url, URL}, MochiReq) when element(1, MochiReq) =:= mochiweb_request ->
    do_be_req(get, URL, MochiReq).
    

do_be_req(Method, BEUrl, MochiReq) ->
    ReqHdrs = mochiweb_headers:to_list(MochiReq:get(headers)),
    case ibrowse:send_req(
           BEUrl, ReqHdrs, Method, [],
           [{stream_to, self()}]) of
        {ibrowse_req_id, ReqId} ->
            request_started(BEUrl, MochiReq, ReqId);
        Err = {error, _} ->
            Err
    end.


request_started(BEUrl, MochiReq, ReqId) ->
    receive
        {ibrowse_async_headers, ReqId, StatusCodeString, RespHeaders} ->
            StatusCode = list_to_integer(StatusCodeString),
            handle_stream_start(MochiReq, ReqId, {StatusCode, RespHeaders})

    after 20000 ->
            error_logger:error_msg("20sec timeout on request to backend ~p~n", [BEUrl]),
            {error, timeout}
    end.

handle_stream_start(MReq, ReqId, {StatusCode, RespHeaders}) ->
    case proplists:get_value("X-Reproxy-Url", RespHeaders) of
        undefined ->
            MResp = MReq:start_response({StatusCode,
                                         RespHeaders}),
            receive_loop(MReq, MResp, ReqId);
        Reproxy ->
            % We no longer need the original Backend request
            % sadly there's no way to cancel a request in ibrowse

            % Flush any leftover messages
            flush_http_req(ReqId),
            {ok, {reproxy, Reproxy}}
    end.

receive_loop(MReq, MResp, ReqId) ->
    receive
        {ibrowse_async_response, ReqId, Body} ->
            MResp:send(Body),
            receive_loop(MReq, MResp, ReqId);

        {ibrowse_async_response_end, ReqId} ->
            {ok, done};

        Else ->
            error_logger:error_msg("ignoring other message: ~p~n", [Else]),
            receive_loop(MReq, MResp, ReqId)
            
    after 10000 ->
            {error, timeout}
    end.


flush_http_req(ReqId) ->
    receive
        {ibrowse_async_response, ReqId, _} ->
            flush_http_req(ReqId);
        {ibrowse_async_response_end, ReqId} ->
            flush_http_req(ReqId)
    after 0 ->
            ok
    end.
