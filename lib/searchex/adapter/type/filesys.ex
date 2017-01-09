 defmodule Searchex.Adapter.Type.Filesys do
#   use Searchex.Adapter

  defstruct type:         "Filesys"                 ,
            file_roots:   []                        ,
            file_types:   ~w(txt md js exs ex)      ,
            file_maxnum:  100                       ,
            file_maxkb:   200                       ,
            file_depth:   2                         ,
            file_skips:   ~w(^\\..+ ^\_ deps docs)

  def init(_args) do
  end

  def default_params do
    %__MODULE__{}
  end

  def pull do
  end

  def events(frame) do
    alias Searchex.Command.Build.Catalog.Params
    frame
    |> Searchex.Command.CmdHelpers.file_list
    |> Util.Ext.File.ls_r(Params.file_params(frame.params))
  end

  def rawdata(filename, frame) do
    maxkb   = frame.params.adapter.file_maxkb
    File.stream!(filename, [], maxkb * 1024) |> Enum.at(0)
  end

  def validate(frame, opts) do
    Searchex.Adapter.Type.Validate.Filesys.call(frame, opts)
  end
 end