defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  defstruct collection:   ""                 ,
            docsep:       ~r/---/            ,
            doc_dirs:     [ ]                ,
            file_types:   ~w(txt md js rb ex),
            field_defs:   %{}                ,
            max_numfiles: 500                ,
            max_file_kb:  500                ,
            cli_format:   %{}

  def create_from_cfg(config) do
    old_docsep = config[:docsep] || "-x-x-"
    new_docsep = regify(old_docsep)
    new_config = Map.merge(config, %{docsep: new_docsep})
    result = plain_struct()
             |> Searchex.Util.Map.deep_merge(new_config)
    Map.merge(%Searchex.Command.Build.Catalog.Params{}, result)
  end

  def regify(elem) when is_map(elem)   , do: elem
  def regify(elem) when is_binary(elem) do
    {:ok, reg} = Regex.compile(elem)
    reg
  end
  def regify(elem), do: elem

  defp plain_struct do
    Map.from_struct %Searchex.Command.Build.Catalog.Params{}
  end






end

