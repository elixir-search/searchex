defmodule Searchex.Command.Build.Catalog do

  @moduledoc false

  defstruct numdocs:       0                                         ,
            avg_wordcount: 0                                         ,
            filescans:     []                                        ,
            docs:          []

  alias Searchex.Command.Build.Catalog
  alias Searchex.Command.Build.Catalog.Doc
  alias Searchex.Command.Build.Catalog.Filescan

  def create_from_frame(frame) do
    %Catalog{}
    |> gen_filescans(frame.params)
    |> gen_docs(frame.params)
    |> add_catids
    |> extract_counts
  end

  defp gen_filescans(catalog, params) do
    scans = params.file_paths
            |> Util.Ext.File.ls_r(params.file_types)
            |> Enum.take(params.max_numfiles)
            |> Enum.map(fn(filename) -> Filescan.generate_filescan(filename, params) end)
    %Catalog{catalog | filescans: scans}
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
