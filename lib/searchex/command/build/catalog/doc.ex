defmodule Searchex.Command.Build.Catalog.Doc do

  @moduledoc false

  defstruct docid:      ""   ,
            filename:   ""   ,
            docstart:   0    ,
            doclength:  0    ,
            wordcount:  0    ,
            wordstems:  []   ,
            fields:     %{}  , 
            body:       ""

  def generate_from_catalog(catalog, params) do
    catalog.filescans
    |> extract_docs
    |> extract_fields(params.field_defs)
  end

  defp extract_docs(filescans) do
    filescans
    |> Enum.flat_map(fn(filescan) -> gen_docs(filescan) end)
  end

  defp gen_docs(filescan) do
    positions = filescan.docsep_positions
    offsets   = filescan.docsep_offsets
    pairs     = List.zip([positions, offsets])
    Enum.reduce setpairs(pairs), [], fn(pair, acc) -> acc ++ [gen_doc(pair, filescan)] end
  end

  defp setpairs([]) , do: [{0, 99999999999}]
  defp setpairs(val), do: val

  defp gen_doc({position, offset}, filescan) do
    body = String.slice(filescan.rawdata, position, offset)
    %Searchex.Command.Build.Catalog.Doc {
      docid:     X.Term.digest(body)                  ,
      filename:  filescan.input_filename              ,
      docstart:  position                             ,
      doclength: offset                               ,
      wordcount: Searchex.Util.String.wordcount(body) ,
      wordstems: Searchex.Util.String.wordstems(body) ,
      body:      body
    }
  end

  defp extract_fields(docs, field_defs) do
    docs
    |> Enum.reduce({[], %{}}, fn(doc, acc) -> get_fields(doc, acc, field_defs) end)
    |> elem(0)
  end

  defp get_fields(doc, {doclist, old_fields}, field_defs) do
    reg_fields = Enum.map field_defs, fn({field_name, _field_spec}) ->
      {field_name, reg_field(field_defs, doc, field_name)}
    end
    new_fields = Map.merge(old_fields, Enum.into(reg_fields, %{}))
    new_doc    = %Searchex.Command.Build.Catalog.Doc{doc | fields: new_fields}
    {doclist ++ [new_doc], new_fields}
  end

  defp reg_field(field_defs, doc, field_name) do
    regstr  = field_defs[field_name].regex
    if caps = Regex.named_captures(~r/#{regstr}/, doc.body) do
      [head | _tail] = Map.values caps
      head
    else
       nil
    end
  end
end
