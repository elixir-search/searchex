defmodule Searchex.Command.Build.Catalog do

  @moduledoc false

  defstruct numdocs:       0                                         ,
            avg_wordcount: 0                                         ,
            filescans:     []                                        ,
            docs:          []

  alias Searchex.Command.Build.Catalog
  alias Searchex.Command.Build.Catalog.Filescan

  def create_from_frame(frame) do
    %Catalog{}
    |> gen_filescans(frame.params)
    |> gen_docs(frame.params)
    |> extract_counts
  end

  def gen_filescans(catalog, params) do
    scans = params.doc_dirs
            |> Searchex.Util.File.ls_r(params.file_types)
            |> Enum.take(params.max_numfiles)
            |> Enum.map(fn(filename) -> Filescan.generate_filescan(filename, params) end)
    %Catalog{catalog | filescans: scans}
  end

  def gen_docs(catalog, params) do
    docs = Searchex.Command.Build.Catalog.Doc.generate_from_catalog(catalog, params)
    %Catalog{catalog | docs: docs}
  end

  def extract_counts(catalog) do
    wordcounts = Enum.map(catalog.docs, fn(doc) -> doc.wordcount end)
    %Catalog{catalog | numdocs: Enum.count(wordcounts), avg_wordcount: Searchex.Util.Enum.average(wordcounts)}
  end
end
