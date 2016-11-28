#defmodule AsyncSup do
#  use Supervisor
#
#  @SERVER __MODULE__
#
#  def start_link do
#    Supervisor.start_link({local, @SERVER}, __MODULE__, [])
#  end
#
#  def init([]) do
#    restart_strategy     = one_for_one
#    max_restarts         = 1000
#    max_sec_bet_restarts = 3600
#    sup_flags            = {restart_strategy, max_restarts, max_sec_bet_restarts}
#
#    restart  = permanent
#    shutdown = 2000
#    type     = worker
#
#    keyword_sup = {keyword_sup, {keyword_sup, start_link, []}, restart, shutdown, type, [keyword_sup]}
#
#    {ok, {sup_flags, [keyword_sup]}}
#  end
#end

# facet_sup          = {facet_sup, {facet_sup, start_link, []}, restart, shutdown, type, [facet_sup]}
# stackov_import_srv = {stackof_importer_ser, {stackof_importer_ser, start_link, []} restart, shutdown, type, [stackof_importer_ser]}
# document_ser       = {document_ser, {document_ser, start_link, []}, restart, shutdown, type, [document_ser]}
# query_ser          = {query_ser, {query_ser, start_link, []}, restart, shutdown, type, [query_ser]}
# web_socket_ser     = {websocket_ser, {websocket_ser, start_link, []}, restart, shutdown, type, [websocket_ser]}

