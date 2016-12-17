defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  defstruct collection:   ""                  ,
            docsep:       nil                 ,
            doc_dirs:     []                  ,
            file_types:   ~w(txt md js exs ex),
            input_fields: %{}                 ,
            max_numfiles: 200                 ,
            max_file_kb:  200                 ,
            cli_format:   %{}

  alias Searchex.Command.Build.Catalog.Params

  def create_from_cfg(config) do
    old_docsep = config[:docsep]
    new_docsep = regify(old_docsep)
    new_config = Map.merge(config, %{docsep: new_docsep})
    result = plain_struct()
             |> Util.Ext.Map.deep_merge(new_config)
    Map.merge(%Searchex.Command.Build.Catalog.Params{}, result)
    |> expand_doc_dirs
  end

  defp expand_doc_dirs(params) do
    new_dirs = Enum.map(params.doc_dirs, fn(dir) -> Path.expand(dir) end)
    %Params{params | doc_dirs: new_dirs}
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

