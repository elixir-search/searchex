defmodule Searchex.KeywordSer do

  @moduledoc false

  use GenServer

  @doc """
  Start KeywordSer

  We start one KeywordSer for each keyword in the index.  The state looks like:

  %{"DIOCID1" => [list of positions], "DIOCID2" => [list of positions]}
  """
  def start_link(server_name \\ :server) do
    GenServer.start_link(__MODULE__, %{}, name: server_name)
  end

  def add_keyword_position(keyword, docid, position) do
    server = get_keyword_server(keyword)
    GenServer.cast(server, {:add, docid, position})
  end

  @doc """
  Perform a query

  terms is a list of strings = ["term1", "term2"]

  doc_matches looks like:
  [{"term1", %{"docid1" => [pos1, pos2], "docid2" => [pos3, pos4]}}, {"term2", %{}}]

  matches_per_term_and_doc looks like:
  %{{"term1", "docid1"} => 23, {"term1", "docid2"} => 4, ...}
  """
  def do_query(terms) when is_list(terms) do
    doc_matches = gen_doc_matches(terms)
    matches_per_term_and_doc = gen_matches_per_term_and_doc(doc_matches)
    res = Searchex.Search.Query.Bm25.doc_scores(terms, doc_matches, matches_per_term_and_doc)
    DIO.inspect :RESULTS
    DIO.inspect res
  end

  def gen_doc_matches(terms) do
    Enum.map(terms, fn(term) -> get_ids(term) end)
  end

  def gen_matches_per_term_and_doc(doc_matches) do
    Enum.reduce doc_matches, %{}, fn({term, docids}, acc1) ->
      tmp = Enum.reduce docids, %{}, fn({docid, pos_list}, acc2) ->
        Map.merge(acc2, %{{term, docid} => Enum.count(pos_list)})
      end
      Map.merge(acc1, tmp)
    end
  end

  @doc """
  Gets the list of ID's for a term.

  The returned data structure look like:
  %{"DIOCID1" => [list of positions], "DIOCID2" => [list of positions]}
  """
  def get_ids(term) do
    case find_keyword_server(term) do
      { :ok, server } -> {term, GenServer.call(server, :get_ids)};
      { :error, _ }   -> {term, %{} }
    end
  end

  @doc """
  Sets the state for a KeywordSer.  This is useful when building an in-memory
  index from a persisted index saved to disk.

  The state looks like:
  %{"DIOCID1" => [list of positions], "DIOCID2" => [list of positions]}
  """
  def set_state(server, new_state) do
    GenServer.cast(server, {:set_state, new_state})
  end

  # --- CALLBACKS

  def init(_opts) do
    {:ok, %{}}
  end

  def handle_call(:get_ids, _from, state) do
    result = state
    {:reply, result, state}
  end

  def handle_cast({:add, docid, position}, state) do
    old_list  = state[docid] || []
    new_list  = old_list ++ [position]
    new_state = Map.merge(state, %{docid => new_list})
    {:noreply, new_state}
  end

  def handle_cast({:set_state, new_state}, _state) do
    {:noreply, new_state}
  end

  def get_keyword_server(keyword) do
    name = cond do
      is_atom(keyword) -> keyword
      true             -> keyword_server_name(keyword)
    end
    Process.whereis(name) || Searchex.KeywordSup.add_child_and_return_pid(name)
  end

  def find_keyword_server(keyword) do
    name = keyword_server_name(keyword)
    case Process.whereis(name) do
      nil -> {:error, "Not found"}
      pid -> {:ok, pid}
    end
  end

  def keyword_server_name(keyword) do
    "kw_" <> keyword |> String.to_atom
  end
end
