defmodule Searchex.Keyword.Server do

  @moduledoc false

  use GenServer

  @doc """
  Keyword Server

  We start one KeywordSer for each keyword in the index.  The state looks like:

  %{"DIOCID1" => [list of positions], "DIOCID2" => [list of positions]}
  """
  def start_link(server_name \\ :server) do
    GenServer.start_link(__MODULE__, %{}, name: server_name)
  end

  def add_keyword_position(pt_name, keyword, docid, position) do
    server = get_keyword_server(pt_name, keyword)
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
  def do_query({pt_name, terms}) when is_list(terms) do
    stem_terms               = Enum.map(terms, &(StemEx.stem(&1)))
    doc_matches              = gen_doc_matches(pt_name, stem_terms)
    matches_per_term_and_doc = gen_matches_per_term_and_doc(doc_matches)
    Searchex.Command.Search.Bm25.doc_scores(terms, doc_matches, matches_per_term_and_doc)
  end
  def do_query({pt_name, terms}) when is_list(terms)   , do: do_query(pt_name, terms)
  def do_query({pt_name, terms}) when is_binary(terms) , do: do_query(pt_name, String.split(terms))
  def do_query({pt_name, terms})                       , do: do_query(pt_name, terms)
  def do_query(pt_name, terms)   when is_binary(terms) , do: do_query(pt_name, String.split(terms, " "))

  def gen_doc_matches(pt_name, terms) do
    Enum.map(terms, fn(term) -> get_ids(pt_name, term) end)
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
  def get_ids(pt_name, term) do
    case find_keyword_server(pt_name, term) do
      {:ok, server} -> {term, GenServer.call(server, :get_ids)};
      {:error, _  } -> {term, %{}}
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

  def get_keyword_server(pt_name, keyword) do
    name = if is_atom(keyword), do: keyword, else: keyword_server_name(pt_name, keyword)
    Process.whereis(name) || Searchex.Keyword.Supervisor.add_child_and_return_pid(pt_name, name)
  end

  def find_keyword_server(pt_name, keyword) do
    name = keyword_server_name(pt_name, keyword)
    case Process.whereis(name) do
      nil -> {:error, "Not found"}
      pid -> {:ok, pid}
    end
  end

  def keyword_server_name(pt_name, keyword) do
    new_pt_name = to_string(pt_name)
    "kw_" <> new_pt_name <> "_" <> keyword |> String.to_atom
  end
end
