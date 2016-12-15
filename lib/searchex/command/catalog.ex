defmodule Searchex.Command.Catalog do

  @moduledoc false

  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params and Catalog slots filled.
  """
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Params
  step :generate_catalog
#  step :generate_digest

  # check to see if the catalog is in the LRU cache, otherwise regenerate
  def generate_catalog(frame, _opts) do
    child_digest = "cat_" <> Frame.get_digest(frame, :params)
    if val = Util.Cache.get_cache(child_digest) do
      cat1 = val
      %Frame{frame | catalog: cat1}
    else
      cat2 = frame |> Searchex.Command.Build.Catalog.create_from_frame
      Util.Cache.put_cache(child_digest, cat2)
      %Frame{frame | catalog: cat2}
    end
  end

  # generate a digest for the catalog and store it in the frame
  def generate_digest(%Frame{cfg_name: cfg_name, catalog: catalog} = frame, _opts) do
    digest = Util.Ext.Term.digest({cfg_name, catalog})
    set_digest(frame, :catalog, digest)
  end
end