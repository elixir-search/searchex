defmodule Searchex.Request.Query do

  @moduledoc false

  use Shreq.Module
  alias Shreq.Frame

  @doc "Module API"
  def exec(cfg_snip, query) do
    call(%Frame{cfg_snip: cfg_snip, query: query}, [])
  end

  step Searchex.Request.Index
  step :do_query
  step :gen_results

  def do_query(%Frame{index: index, query: query} = frame, _opts) do
    query_digest = Util.Ext.Term.digest(query)
    param_digest = Frame.get_digest(frame, :params)
    child_digest = "qry_#{frame.cfg_name}_#{query_digest}_#{param_digest}"
    if scores1 = Util.Cache.get_cache(frame, child_digest) do
      %Frame{frame | scores: scores1}
      |> set_digest(:scores, Util.Ext.Term.digest(scores1))
    else
      scores2 = {index, String.split(query)} |> exec_query(frame)
      Util.Cache.put_cache(frame, "#{frame.cfg_name}_last_query", query)
      Util.Cache.put_cache(frame, child_digest, scores2)
      %Frame{frame | scores: scores2}
      |> set_digest(:scores, Util.Ext.Term.digest(scores2))
    end
  end

  def gen_results(%Frame{catalog: catalog, scores: scores} = frame, _opts) do
    child_digest = "qry_#{frame.cfg_name}_#{Frame.get_digest(frame, :scores)}"
    if results1 = Util.Cache.get_cache(frame, child_digest) do
      %Frame{frame | results: results1}
    else
      results2 = filter_docs_by_scores(catalog, scores)
      Util.Cache.put_cache(frame, "#{frame.cfg_name}_last_results", results2)
      %Frame{frame | results: results2}
    end
  end

  # -----

  defp filter_docs_by_scores(catalog, scores) do
    alias Searchex.Request.Build.Catalog.Doc
    docids   = Enum.map scores, &(elem(&1, 0))
    scoremap = Enum.into(scores, %{})
    new_docs = catalog.docs
               |> Enum.filter(fn(x) -> Enum.member?(docids, x.docid) end)
               |> Enum.sort_by(fn(x) -> -1 * scoremap[x.docid] end)
               |> Enum.slice(0, 20)
               |> Enum.map(fn(doc) -> %Doc{doc | score: scoremap[doc.docid]} end)
    %Searchex.Request.Build.Catalog{catalog | docs: new_docs}
  end

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