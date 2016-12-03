defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  @date_reg ~r/(?<date>(MON|TUE|WED|THU|FRI|SAT|SUN) [01][0-9]\-[01][0-9]\-[0-3][0-9])/

  defstruct collection:   ""               ,
            docsep:       ~r/---/          ,
            doc_dirs:     [ ]              ,
            file_types:   [ ]              ,
            field_defs:   %{}              ,
            max_numfiles: 500              ,
            max_file_kb:  500              ,
            cli_format:   %{}

  def create_from_cfg(config) do
#    new_config = %{config | docsep: ~r/(\n---\n)|(\n\*\*\n)/}
    result = plain_struct()
             |> Searchex.Util.Map.deep_merge(config)
    Map.merge(%Searchex.Command.Build.Catalog.Params{}, result)
  end

  defp plain_struct do
    Map.from_struct %Searchex.Command.Build.Catalog.Params{}
  end
end

