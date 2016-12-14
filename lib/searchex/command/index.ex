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

  # TODO: reload the index from cache, if possible
  def generate_index(frame, _opts) do
    Searchex.Command.Build.Index.create_from_frame(frame)
    #X.Cache.put_cache(frame.digests.catalog, arg)
    %Frame{frame | index: frame.cfg_name}
  end
end