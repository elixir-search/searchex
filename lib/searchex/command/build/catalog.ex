defmodule Searchex.Command.Build.Catalog do

  @moduledoc false

  defstruct params:        %Searchex.Command.Build.Catalog.Params{}  ,
            numdocs:       0                                         ,
            avg_wordcount: 0                                         ,
            filescans:     []                                        ,
            docs:          []

  def create_from_params(params) do
    %Searchex.Command.Build.Catalog{params: params}
    |> gen_filescans
    |> gen_docs
    |> extract_counts
  end

  def gen_filescans(catalog) do
    scans = catalog.params.doc_dirs
            |> Searchex.Util.File.ls_r(catalog.params.file_types)
            |> Enum.take(catalog.params.max_numfiles)
            |> Enum.map(fn(filename) -> Searchex.Command.Build.Catalog.Filescan.generate_filescan(catalog, filename) end)
    %Searchex.Command.Build.Catalog{catalog | filescans: scans}
  end

  def gen_docs(catalog) do
    docs = Searchex.Command.Build.Catalog.Doc.generate_from_catalog(catalog)
    %Searchex.Command.Build.Catalog{catalog | docs: docs}
  end

  def extract_counts(catalog) do
    wordcounts = Enum.map(catalog.docs, fn(doc) -> doc.wordcount end)
    newvals    = %{numdocs: Enum.count(wordcounts), avg_wordcount: Searchex.Util.Enum.average(wordcounts)}
    Map.merge(catalog, newvals)
  end
end
