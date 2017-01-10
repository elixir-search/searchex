 defmodule Searchex.Adapter.Type.Filesys do

  def init(_args) do
  end

  def default_settings do
    %{
      type:         "Filesys"                 ,
      file_roots:   []                        ,
      file_types:   ~w(txt md js exs ex)      ,
      file_maxnum:  100                       ,
      file_maxkb:   200                       ,
      file_depth:   2                         ,
      file_skips:   ~w(^\\..+ ^\_ deps docs)
      }
  end

  def cursor(_frame) do
  end

  def events(frame) do
    frame
    |> file_list
    |> Util.Ext.File.ls_r(file_params(frame.params))
  end

  def rawdata(filename, frame) do
    maxkb = frame.params.adapter.file_maxkb
    File.stream!(filename, [], maxkb * 1024) |> Enum.at(0)
  end

  def shake(frame, opts) do
    Searchex.Adapter.Shake.Filesys.call(frame, opts)
  end

  # -----

  defp file_list(frame) do
    alias Searchex.Command.CmdHelpers
    absolute_roots = CmdHelpers.expanded_file_roots(frame)
    Util.Ext.File.ls_r absolute_roots, file_params(frame.params)
#    Util.Ext.File.ls_r absolute_roots, frame.params.adapter
  end

  def file_params(params) do
    %{
      types:  params.adapter.file_types   ,
      skips:  params.adapter.file_skips   ,
      maxnum: params.adapter.file_maxnum  ,
      depth:  params.adapter.file_depth   ,
      maxkb:  params.adapter.file_maxkb
    }
  end
end