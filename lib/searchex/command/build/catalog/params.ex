defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  defstruct collection:   ""                        ,
            cache_dir:    "."                       ,
            docsep:       nil                       ,
            adapter:      %{}                       ,
            input_fields: %{}                       ,
            cli_format:   %{}

  alias Searchex.Command.Build.Catalog.Params

  def create_from_cfg(config) do
    Map.merge(%Params{}, config)
  end
end
