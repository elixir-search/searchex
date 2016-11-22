-module(async_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    KeywordSup = {keyword_sup, {keyword_sup, start_link, []},
	      Restart, Shutdown, Type, [keyword_sup]},
    FacetSup = {facet_sup, {facet_sup, start_link, []},
	      Restart, Shutdown, Type, [facet_sup]},
    StackOverflowImporterSer = {stackoverflow_importer_ser, {stackoverflow_importer_ser, start_link, []},
	      Restart, Shutdown, Type, [stackoverflow_importer_ser]},
    DocumentSer = {document_ser, {document_ser, start_link, []},
	      Restart, Shutdown, Type, [document_ser]},
    QuerySer = {query_ser, {query_ser, start_link, []},
	      Restart, Shutdown, Type, [query_ser]},
    WebSocketSer = {websocket_ser, {websocket_ser, start_link, []},
	      Restart, Shutdown, Type, [websocket_ser]},

    {ok, {SupFlags, [KeywordSup, FacetSup, StackOverflowImporterSer, DocumentSer, QuerySer, WebSocketSer]}}.

