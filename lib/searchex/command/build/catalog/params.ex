defmodule Searchex.Command.Build.Catalog.Params do

  @moduledoc false

  @adapter_default %{
    file_roots:       []                        ,
    file_types:       ~w(txt md js exs ex)      ,
    file_maxnum:      100                       ,
    file_depth:       2                         ,
    file_skips:       ~w(^\\..+ ^\_ deps docs)  ,
    bucket_maxkb:     100
  }

  defstruct collection:   ""                        ,
            cache_dir:    "."                       ,
            file_roots:   []                        ,
            file_types:   ~w(txt md js exs ex)      ,
            file_maxnum:  100                       ,
            bucket_maxkb: 100                       ,
            file_depth:   2                         ,
            file_skips:   ~w(^\\..+ ^\_ deps docs)  ,
            docsep:       nil                       ,
            adapter:      @adapter_default          ,
            input_fields: %{}                       ,
            cli_format:   %{}

  alias Searchex.Command.Build.Catalog.Params

  def create_from_cfg(config) do
    Map.merge(%Params{}, config)
  end

  def file_params(params) do
    %{
      types:  params.adapter.file_types   ,
      skips:  params.adapter.file_skips   ,
      maxnum: params.adapter.file_maxnum  ,
      depth:  params.adapter.file_depth   ,
      maxkb:  params.bucket_maxkb
    }
  end
end
