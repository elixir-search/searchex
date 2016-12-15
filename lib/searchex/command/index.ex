defmodule Searchex.Command.Index do

  @moduledoc false

  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with Params, Catalog and Index filled.
  """
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Catalog
  step :generate_index
#  step :generate_digest

  def generate_index(frame, _opts) do
    child_digest = "idx_" <> Frame.get_digest(frame, :params)
    if _map = Util.Cache.get_cache(child_digest) do
      Searchex.Command.Build.Index.create_from_frame(frame)
      %Frame{frame | index: frame.cfg_name}
    else
      Searchex.Command.Build.Index.create_from_frame(frame)
      #Util.Cache.put_cache(frame.digests.catalog, arg)
      %Frame{frame | index: frame.cfg_name}
    end
  end
end

# TODO: finish this