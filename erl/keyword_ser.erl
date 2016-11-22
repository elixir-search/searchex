-module(keyword_ser).
-behaviour(gen_server).

%% API
-export([start_link/1, add_keyword_position/3, do_query/1, get_ids/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {document_positions}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).
    

add_keyword_position(Keyword, DocCatId, Position) ->
    Server = get_keyword_server( Keyword ),
    gen_server:cast(Server, {add, DocCatId, Position}).

do_query(Terms) when is_list(Terms) ->
    DocumentMatches = lists:flatten(rpc:pmap( { keyword_ser, get_ids }, [], Terms)),
    MatchesPerTermAndDocument = lists:foldl( 
				  fun( {Term, DocIds}, AccIn1) ->
					  lists:foldl(fun(DocId, AccIn2) -> dict:update_counter( {Term, DocId}, 1, AccIn2) end, AccIn1, DocIds)
				  end, dict:new(), DocumentMatches),
    async_bm25:document_scores( Terms, DocumentMatches, MatchesPerTermAndDocument).

get_ids(Term)->
    case find_keyword_server( Term ) of
	{ ok, Server } ->
	    { Term, gen_server:call( Server, get_ids )};
	{ error, _ } ->
	    { Term, []}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{document_positions=dict:new()}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
handle_call(get_ids, _From, State) ->
    Reply = dict:fold(
	      fun( DocId, Positions, AccIn) -> 
		      lists:duplicate(length( Positions ), DocId) ++ AccIn 
	      end,
	      [], State#state.document_positions
	     ),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%%--------------------------------------------------------------------
handle_cast({add, DocCatId, Position}, State ) ->
    NewState = #state{ document_positions = dict:append( DocCatId, Position, State#state.document_positions ) },
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_keyword_server(Keyword) ->
    ServerName = keyword_server_name(Keyword),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
            {ok, Pid} = supervisor:start_child(keyword_sup, [ServerProcessName]);
        Pid ->
            Pid
    end,
    Pid.

find_keyword_server(Keyword) ->
    ServerName = keyword_server_name(Keyword),
    ServerProcessName = list_to_atom(ServerName),
    case whereis(ServerProcessName) of
	undefined ->
	    { error, "Not found" };
        Pid ->
            { ok, Pid }
    end.    

keyword_server_name(Keyword) ->
    integer_to_list(
      erlang:phash2(
	porter:stem(
	  string:to_lower(Keyword)
	 )
       )
     ) ++ "_keyword_server".
