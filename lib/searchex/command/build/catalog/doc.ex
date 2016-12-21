defmodule Searchex.Command.Build.Catalog.Doc do

  @moduledoc false

  defstruct docid:      ""   ,
            catid:      0    ,
            fileid:     0    ,
            filename:   ""   ,
            startline:  0    ,
            startbyte:  0    ,
            doclength:  0    ,
            wordcount:  0    ,
            wordstems:  []   ,
            fields:     %{}  , 
            body:       ""

  alias Searchex.Command.Build.Catalog.Doc

  @max_doclen 99999999999

  def generate_from_catalog(catalog, params) do
    catalog.filescans
    |> extract_docs
    |> extract_fields(params.input_fields)
  end

  defp extract_docs(filescans) do
    filescans
    |> Enum.flat_map(fn(filescan) -> gen_docs(filescan) end)
  end

  defp gen_docs(filescan) do
    positions = filescan.docsep_positions
    offsets   = filescan.docsep_offsets
    pairs     = List.zip([positions, offsets])
    inputs    = setpairs(pairs) |> Enum.with_index(1)
    Enum.reduce(inputs, [], fn(pair, acc) -> acc ++ [gen_doc(pair, filescan)] end)
    |> Enum.reduce({[], 0}, fn(doc, acc) -> setline(doc, acc) end )
    |> elem(0)
  end

  defp setline(doc, {doclist, count}) do
    newcount = count + countlines(doc.body)
    newdoc   = %Doc{doc | startline: newcount}
    {doclist ++ [newdoc], newcount}
  end

  defp countlines(string) do
    Regex.scan(~r/\n/, string) |> Enum.count
  end

  defp setpairs([]) , do: [{0, @max_doclen}]
  defp setpairs(val), do: val

  defp gen_doc({{position, offset}, idx}, filescan) do
    body = String.slice(filescan.rawdata, position, offset)
    %Doc{
      docid:     Util.Ext.Term.digest(body)      ,
      fileid:    idx                             ,
      filename:  filescan.input_filename         ,
      startbyte: position                        ,
      doclength: set_doclength(offset, body)     ,
      wordcount: Util.Ext.String.wordcount(body) ,
      wordstems: Util.Ext.String.wordstems(body) ,
      body:      body
    }
  end

  defp set_doclength(offset, body) do
    if offset == @max_doclen, do: String.length(body), else: offset
  end

  defp extract_fields(docs, input_fields) do
    docs
    |> Enum.reduce({[], %{}}, fn(doc, acc) -> get_fields(doc, acc, input_fields) end)
    |> elem(0)
  end

  defp get_fields(doc, {doclist, old_fields}, input_fields) do
    alt_input_fields = input_fields || []
    reg_fields = Enum.map alt_input_fields, fn({field_name, _field_spec}) ->
      {field_name, reg_field(input_fields, doc, field_name)}
    end
    new_fields = Map.merge(old_fields, Enum.into(reg_fields, %{}))
    new_doc    = %Doc{doc | fields: new_fields}
    {doclist ++ [new_doc], new_fields}
  end

  defp reg_field(input_fields, doc, field_name) do
    regstr  = input_fields[field_name].regex
    if caps = Regex.named_captures(~r/#{regstr}/, doc.body) do
      [head | _tail] = Map.values caps
      head
    else
       nil
    end
  end
end
