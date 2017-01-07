defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  defstruct collection:   ""                        ,
            cache_dir:    "."                       ,
            file_roots:   []                        ,
            file_types:   ~w(txt md js exs ex)      ,
            file_maxnum:  100                       ,
            bucket_maxkb:   100                       ,
            file_depth:   2                         ,
            file_skips:   ~w(^\\..+ ^\_ deps docs)  ,
            docsep:       nil                       ,
            adapter:      %{}                       ,
            input_fields: %{}                       ,
            cli_format:   %{}

  alias Searchex.Command.Build.Catalog.Params

  def create_from_cfg(config) do
    Map.merge(%Params{}, config)
  end

  def file_params(params) do
    %{
      types:  params.file_types   ,
      skips:  params.file_skips   ,
      maxnum: params.file_maxnum  ,
      maxkb:  params.bucket_maxkb   ,
      depth:  params.file_depth
    }
  end
end
