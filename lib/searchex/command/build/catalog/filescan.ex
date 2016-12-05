defmodule Searchex.Command.Build.Catalog.Filescan do
  @moduledoc false
  defstruct params:   %Searchex.Command.Build.Catalog.Params{}  ,
            rawdata:  ""                                        ,
            input_filename:    ""                               ,
            numdocs:           0                                ,
            avg_wordcount:     0                                ,
            docsep_positions:  []                               ,
            docsep_offsets:    []                               ,
            docs:              []

  @doc "Add a new document to the scan"
  def add_doc(scan, newdoc) do
    newdocs = scan.docs ++ [newdoc]
    %Searchex.Command.Build.Catalog.Filescan{scan | docs: newdocs}
  end

  @doc "Create a new Scan with specified Params"
  def create_from_params(params) do
    %Searchex.Command.Build.Catalog.Filescan{params: params}
  end

  @doc "Generate table data from the scan"
  def table_data(docs, opts \\ [title: "Collection", fields: ~w(filename doclength docstart body)]) do
    fields  = ~w(docid) ++ opts[:fields]
    headers = Enum.map fields, &clean_header(&1)
    rows    = docs
              |> Enum.with_index(1)
              |> Enum.map(&row_data(&1, fields))
    {opts[:title], ["ID"] ++ headers, rows}
  end

  # ---------------------------------------------------------------------------------------------

  defp clean_header(header) do
    header
    |> String.replace("f:", "")
    |> String.capitalize
  end

  defp row_data({doc, idx}, headers) do
    [idx] ++ Enum.map headers, &row_item(doc, &1)
  end

  defp row_item(doc, <<"f:" , field_name::binary>>) do
    Map.get(doc.fields, String.to_atom(field_name)) || "TBD"
  end

  defp row_item(doc, header_name) do
    value = Map.get(doc, String.to_atom(header_name)) || "TBD"
    "#{value}"
    |> String.slice(0..40)
    |> String.replace(~r/[\j\n\m\r\e\a\f\t\v]/, " ")
    |> String.replace(~r/[\x80-\xff]/, "")
    |> String.slice(0..30)
  end
end
