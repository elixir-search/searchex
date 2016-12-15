defmodule Searchex.Command.Query do

  @moduledoc false

  use Shake.Module

  @doc "Module API"
  def exec(cfg_name, query) do
    call(%Frame{cfg_name: cfg_name, query: query}, [])
  end

  step Searchex.Command.Index
  step :do_query
  step :gen_results

  def do_query(%Frame{cfg_name: cfg_name, query: query} = frame, _opts) do
    scores = {cfg_name, String.split(query)}
             |> Searchex.Keyword.Server.do_query
    %Frame{frame | scores: scores}
  end

  def gen_results(%Frame{catalog: catalog, scores: scores} = frame, _opts) do
    %Frame{frame | results: filter_docs_by_scores(catalog, scores)}
  end

  defp filter_docs_by_scores(catalog, scores) do
    docids   = Enum.map scores, &(elem(&1, 0))
    new_docs = catalog.docs
               |> Enum.filter(fn(x) -> Enum.member?(docids, x.docid) end)
               |> Enum.sort_by(fn(x) -> Enum.find_index(docids, fn(y) -> x == y end) end)
               |> Enum.slice(0, 20)
    %Searchex.Command.Build.Catalog{catalog | docs: new_docs}
  end
end
