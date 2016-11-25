defmodule Searchex.Build.Catalog.Params do

  @moduledoc false

  alias Searchex.Util.IO, as: DIO

  @date_reg ~r/(?<date>(MON|TUE|WED|THU|FRI|SAT|SUN) [01][0-9]\-[01][0-9]\-[0-3][0-9])/

  defstruct collection:  ""                               ,
            docsep:      ~r/---/                          ,
            doc_dirs:    [ ]                              ,
            file_types:  [ ]                              ,
            field_defs:  %{}

  def create_from_cfg(config) do
    DIO.inspect CREATEFROM: config
    new_config = %{config | docsep: ~r/(\n---\n)|(\n\*\*\n)/}
    DIO.inspect AFTERALL: new_config
    # DIO.inspect new_config["docsep"]
    result     = plain_struct()
      |> Searchex.Util.Map.deep_merge(new_config)
    Map.merge(%Searchex.Build.Catalog.Params{}, result)
  end

  defp plain_struct do
    Map.from_struct %Searchex.Build.Catalog.Params{}
  end
end

