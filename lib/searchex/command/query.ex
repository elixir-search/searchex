defmodule Searchex.Command.Query do

  @moduledoc false

  use Shake.Module
  alias Shake.Frame

  @doc "Module API"
  def exec(cfg_snip, query) do
    call(%Frame{cfg_snip: cfg_snip, query: query}, [])
  end

  step Searchex.Command.Index
  step :do_query
  step :gen_results

  def do_query(%Frame{index: index, query: query} = frame, _opts) do
    query_digest = Util.Ext.Term.digest(query)
    child_digest = "qry_#{frame.cfg_name}_#{query_digest}"
    if scores1 = Util.Cache.get_cache(frame, child_digest) do
      %Frame{frame | scores: scores1}
    else
      scores2 = {index, String.split(query)} |> exec_query(frame)
      Util.Cache.put_cache(frame, "#{frame.cfg_name}_last_query", query)
      Util.Cache.put_cache(frame, child_digest, scores2)
      %Frame{frame | scores: scores2} |> set_digest(:scores, Util.Ext.Term.digest(scores2))
    end
  end

  def gen_results(%Frame{catalog: catalog, scores: scores} = frame, _opts) do
    child_digest = "qry_#{frame.cfg_name}_#{Frame.get_digest(frame, :scores)}"
    if results1 = Util.Cache.get_cache(frame, child_digest) do
      %Frame{frame | results: results1}
    else
      results = filter_docs_by_scores(catalog, scores)
      Util.Cache.put_cache(frame, "#{frame.cfg_name}_last_results", results)
      %Frame{frame | results: results}
    end
  end

  defp filter_docs_by_scores(catalog, scores) do
    docids   = Enum.map scores, &(elem(&1, 0))
    new_docs = catalog.docs
               |> Enum.filter(fn(x) -> Enum.member?(docids, x.docid) end)
               |> Enum.sort_by(fn(x) -> Enum.find_index(docids, fn(y) -> x == y end) end)
               |> Enum.slice(0, 20)
    %Searchex.Command.Build.Catalog{catalog | docs: new_docs}
  end

  # -----

  defp exec_query({index, query_list}, frame) do
    case Enum.at(query_list, 0) do
      "." -> dump_top(frame)
      _   -> Searchex.Keyword.Server.do_query({index, query_list})
    end
  end

  defp dump_top(frame) do
    Enum.slice(frame.catalog.docs, 0..20)
    |> Enum.map(fn(doc) -> {doc.docid, 1.00} end)
  end
end