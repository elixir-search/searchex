% ==========================================================================================================
% MISULTIN - Example: Shows misultin Websocket With an event support.
%
% >-|-|-(°>
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
% All rights reserved.
%
% BSD License
% 
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%    following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%    the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%    products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module(websocket_ser).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([query_results/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE). 
-define(PORT, 3000).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    misultin:start_link([
			 {port, ?PORT}, {loop, fun(Req) -> handle_http(Req, ?PORT) end},
			 {ws_loop, fun(Ws) -> handle_websocket(Ws) end}, {ws_autoexit, false}
			]),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    misultin:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

% callback on request received
handle_http(Req, Port) ->	
	% output
	Req:ok([{"Content-Type", "text/html"}],
	["	
	<html>
		<head>
                        <script type='text/javascript' src='http://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js'></script>
			<script type=\"text/javascript\">
				function addStatus(text){
					document.getElementById('status').innerHTML = document.getElementById('status').innerHTML + Date.now() + \": \" + text + \"<br>\";				
				}
				function submit_it(){
                                        document.getElementById('status').innerHTML = \"\";

					if (\"WebSocket\" in window) {
						// browser supports websockets
						var ws = new WebSocket(\"ws://localhost:", erlang:integer_to_list(Port) ,"/service\");
						ws.onopen = function() {
							// websocket is connected
							addStatus(\"websocket connected!\");
							// send hello data to server.
							ws.send(  document.getElementById('query').value );
							addStatus(\"sent query to server:\" + document.getElementById('query').value);
						};
						ws.onmessage = function (evt) {
							var receivedMsg = evt.data;
							addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
						};
						ws.onclose = function() {
							// websocket was closed
							addStatus(\"websocket was closed\");
						};
					} else {
						// browser does not support websockets
						addStatus(\"sorry, your browser does not support websockets.\");
					}
				}
			</script>
		</head>
		<body>
                        <form method=\"post\" onsubmit=\"submit_it(); return false;\">
                           <input id=\"query\" type=\"text\" value=\"\" placeholder=\"your query\" />
                           <input type=\"submit\" value=\"Submit\" />
                        </form>
			<div id=\"status\"></div>
		</body>
	</html>"]).


query_results({docs, Documents}, [Ws]) ->
    %% create a nice list of results with scores. 
    Ws:send(["got doc results"]),
    Partial = "<li><strong>Id:</strong> ~p<br /><strong>Score:</strong> ~p<br /><strong>Title:</strong> ~ts<br /><strong>Body:</strong> ~ts</li>",
    Html = lists:map( fun({Score, Attributes}) -> io_lib:format(Partial,[dict:fetch("Id", Attributes), Score, dict:fetch("Title", Attributes), dict:fetch("Body", Attributes)]) end, Documents),
    Output = io_lib:format("<p><ol>~ts</ol></p>", [lists:append(Html)]),
    Ws:send(["documents:", unicode:characters_to_binary(Output)]),
    ok;
query_results({facet_results, [{"Tags", Tags}, {"CreationDate", _CreationDate}]}, [Ws]) ->
    Html = lists:map(
	     fun({[Tag], Count}) ->
		     io_lib:format(
		       "<li>~ts ( ~p ) </li>",
		       [Tag, Count]
		     )
	     end,
	     Tags
	    ),
    Output = io_lib:format("<p><ol>~ts</ol></p>", [lists:append(Html)]),
    Ws:send(["tags:", unicode:characters_to_binary(Output)]),
    ok;
query_results(_, [Ws]) ->
    Ws:send(["unrecognized result set"]),
    ok.

% callback on received websockets data
handle_websocket(Ws) ->
	receive
		{browser, Data} ->
			Ws:send(["performing query'", Data, "'"]),
		        query_ser:do_async_query( string:tokens(Data, " "), ["Tags", "CreationDate"], ?MODULE, query_results, [Ws]),
			handle_websocket(Ws);
		closed ->
			% IMPORTANT: since we specified the {ws_autoexit, false} option, we need to manually ensure that this process exits
			% [otherwise it will become a zombie]
			io:format("The WebSocket was CLOSED!~n");
		_Ignore ->
			handle_websocket(Ws)
	end.
