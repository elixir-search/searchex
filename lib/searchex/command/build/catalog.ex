defmodule Searchex.Command.Build.Catalog do

  @moduledoc false

  defstruct numdocs:       0                                         ,
            avg_wordcount: 0                                         ,
            bucketscans:   []                                        ,
            docs:          []

  alias Searchex.Command.Build.Catalog
  alias Searchex.Command.Build.Catalog.Doc
  alias Searchex.Command.Build.Catalog.Bucketscan

  def create_from_frame(frame) do
    %Catalog{}
    |> gen_bucketscans(frame)
    |> gen_docs(frame.params)
    |> add_catids
    |> extract_counts
  end

  defp gen_bucketscans(catalog, frame) do
    scans = frame
            |> Util.Adapter.event_ids(:create)
            |> Task.async_stream(Bucketscan, :generate_bucketscan, [frame])
            |> Enum.to_list()
            |> Enum.map(fn(el) -> elem(el, 1) end)
    %Catalog{catalog | bucketscans: scans}
  end

  defp gen_docs(catalog, params) do
    docs = Searchex.Command.Build.Catalog.Doc.generate_from_catalog(catalog, params)
    %Catalog{catalog | docs: docs}
  end

  defp add_catids(catalog) do
    new_docs = catalog.docs
               |> Enum.with_index(1)
               |> Enum.map(fn({doc, idx}) -> %Doc{doc | catid: idx} end)
    %Catalog{catalog | docs: new_docs}
  end

  defp extract_counts(catalog) do
    wordcounts = Enum.map(catalog.docs, fn(doc) -> doc.wordcount end)
    %Catalog{catalog | numdocs: Enum.count(wordcounts), avg_wordcount: Util.Ext.Enum.average(wordcounts)}
  end
end
