defmodule Searchex.Build.Catalog.Scan do
  @moduledoc false
  defstruct params:   %Searchex.Build.Catalog.Params{}          ,
            rawdata:  ""                                        ,
            input_filename:    ""                               ,
            collection_name:   ""                               ,
            numdocs:           0                                ,
            avg_wordcount:     0                                ,
            docsep_positions:  []                               ,
            docsep_offsets:    []                               ,
            docs:              []

  @doc "Add a new document to the scan"
  def add_doc(scan, newdoc) do
    newdocs = scan.docs ++ [newdoc]
    %Searchex.Build.Catalog.Scan{scan | docs: newdocs}
  end

  @doc "Create a new Scan with specified Params"
  def create_from_params(params) do
    %Searchex.Build.Catalog.Scan{params: params}
  end

  @doc "Generate table data from the scan"
  def table_data(scan, opts \\ [title: "Collection", headers: ~w(filename doclength docstart body)]) do
    headers = Enum.map opts[:headers], &clean_header(&1)
    rows    = Enum.map scan.docs, &row_data(&1, opts[:headers])
    {opts[:title], headers, rows}
  end

  # ---------------------------------------------------------------------------------------------

  defp clean_header(header) do
    header
    |> String.replace("f:", "")
    |> String.capitalize
  end

  defp row_data(doc, headers) do
    Enum.map headers, &row_item(doc, &1)
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
