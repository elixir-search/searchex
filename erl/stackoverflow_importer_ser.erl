%%%-------------------------------------------------------------------
%%% @author Ward Bekker <ward@tty.nl>
%%% @copyright
%%% Copyright (c) 2011 Ward Bekker / TTY Internet Solutions
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%% @end
%%%-------------------------------------------------------------------
-module(stackoverflow_importer_ser).
-behaviour(gen_server).

%% API
-export([start_link/0, import/0]).

%% Internal use only
-export([continue_file/2, sax_event/1, add_attribute_tokens/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(POSTS_PATH, '/resources/medium_posts.xml').

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

import() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    {ok, Dir} = file:get_cwd(),
    gen_server:call(Pid, {import, Dir ++ ?POSTS_PATH}, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({import, FilePath}, _From, State) ->
    {ok, Handle} = file:open(FilePath, [read, raw, binary]),
    Position = 0,
    Chunk = 1024,
    CState = {Handle, Position, Chunk},
    {ok, _Result, _TrailingBytes} = 
    erlsom:parse_sax(<<>>, undefined, fun(Event, _Acc) -> sax_event(Event) end, [{continuation_function, fun continue_file/2, CState}]),
    file:close(Handle),
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% this is a continuation function that reads chunks of data 
%% from a file.
continue_file(Tail, {Handle, Offset, Chunk}) ->
  %% read the next chunk
  case file:pread(Handle, Offset, Chunk) of
    {ok, Data} ->
      {<<Tail/binary, Data/binary>>, {Handle, Offset + Chunk, Chunk}};
    eof ->
      {Tail, {Handle, Offset, Chunk}}
  end.

sax_event({startElement, [] , "row", [], Attributes}) ->
    {_,_,_,_,StringId} = lists:keyfind( "Id", 2, Attributes ),
    Id = erlang:list_to_integer(StringId),
    AttributesDict = lists:foldl(
		       fun( {attribute, AttributeName,_,_,AttributeValue}, AccIn )-> 
			       dict:store(AttributeName, AttributeValue, AccIn) 
		       end, 
		       dict:new(),
		       Attributes),
    CompleteAttributesDict = case dict:is_key("Title", AttributesDict) of
				 true -> AttributesDict;
				 false -> dict:store("Title", "No title present", AttributesDict)
			     end,
    gen_server:cast( document_ser, { add_document, Id, CompleteAttributesDict }),
    dict:map(fun(AttributeName, AttributeValue)-> add_attribute_tokens(Id, AttributeName, AttributeValue) end, CompleteAttributesDict);
sax_event(_Event) ->
    ok.

add_attribute_tokens(Id, AttributeName, AttributeValue) ->
    add_facet( Id, AttributeName, AttributeValue ),
    Tokens = string:tokens(string:to_lower(AttributeValue), " ,.:;-!?"),
    TokensAndPositions = lists:zip( Tokens, lists:seq( 1, length(Tokens))),
    lists:foreach( fun({Token, Position}) -> keyword_ser:add_keyword_position( Token, Id, Position) end, TokensAndPositions).

add_facet(Id, "Tags", AttributeValue) ->
    {match, Captured} = re:run(AttributeValue, "<(.*?)>", [global, {capture, all_but_first, list}]),
    lists:foreach( fun(Match) -> facet_ser:add_facet_value( "Tags", Match, Id) end, Captured);
add_facet(Id, "CreationDate", AttributeValue) ->
    [ Date | _Time ] = string:tokens(AttributeValue, "T"),
    facet_ser:add_facet_value("CreationDate", Date, Id);
add_facet(_,_,_) ->
    ignore.
