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
-module(facet_ser).
-behaviour(gen_server).

%% API
-export([start_link/1, get_facets/2, add_facet_value/3, add_document_to_value/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {valuecounts}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).

get_facets( Category, DocumentIds) ->
    case find_facet_server( Category ) of
	{ error, _ } ->
	    dict:new();
	{ ok, Server } -> gen_server:call( Server, {get_facet_counts, DocumentIds} )
    end.    
    
add_facet_value(Category, Value, DocId) ->
    Server = get_facet_server( Category ),
    gen_server:cast(Server, {add, Value, DocId}).

add_document_to_value( Value, DocId, Dict) ->
    case dict:find(Value, Dict) of
	{ok, DocIds} ->
	    NewSet = sets:add_element(DocId, DocIds);
	error ->
	    NewSet = sets:add_element(DocId, sets:new())
    end,
    dict:store(Value, NewSet, Dict).

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
    {ok, #state{valuecounts=dict:new()}}.

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
handle_call({get_facet_counts, DocumentIds}, _From, State) ->
    %% fold over valuecounts
    %% return dict with set totals
    Results = dict:fold(
	      fun(Key, Set, AccIn) -> 
		      Hits = sets:size( sets:intersection(Set, sets:from_list(DocumentIds))),
		      case Hits of
			  0 -> AccIn;
			  _ -> dict:store(Key, Hits, AccIn)
		      end
	      end,
	      dict:new(),
	      State#state.valuecounts
	     ),
    Reply = lists:reverse(lists:keysort(2, dict:to_list(Results))),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Value, DocId}, State) ->
    %% find or create dict entry for value
    %% add DocId to list
    NewState = #state{ valuecounts = add_document_to_value( Value, DocId, State#state.valuecounts )},
    {noreply, NewState}.

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

find_facet_server(Category) ->
    ServerName = facet_server_name(Category),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
            { error, "Not found"};
        Pid ->
            {ok, Pid}
    end.

get_facet_server(Category) ->
    ServerName = facet_server_name(Category),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
            {ok, Pid} = supervisor:start_child(facet_sup, [ServerProcessName]);
        Pid ->
            Pid
    end,
    Pid.

facet_server_name(Category) ->
    integer_to_list(erlang:phash2(string:to_lower(Category))) ++ "_facet_server".
