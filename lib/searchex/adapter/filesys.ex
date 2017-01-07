 defmodule Searchex.Adapter.Filesys do
#   use Searchex.Adapter

  defstruct collection:   ""                        ,
            file_roots:   []                        ,
            file_types:   ~w(txt md js exs ex)      ,
            file_maxnum:  100                       ,
            bucket_maxkb:   100                       ,
            file_depth:   2                         ,
            file_skips:   ~w(^\\..+ ^\_ deps docs)

  def init(_args) do
  end

  def pull do
  end

  def events(_args) do
  end

  def rawdata(_bucket_id) do
  end

  def validate(_params) do
  end
 end