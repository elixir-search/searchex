defmodule Searchex.Command.Catalog do

  @moduledoc false

  use Shake.Module
  alias Searchex.Command.Build.Catalog

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params and Catalog slots filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step Searchex.Command.Params
  step :generate_catalog

  def generate_catalog(frame, _opts) do
    child_digest = "cat_#{frame.cfg_name}_" <> Frame.get_digest(frame, :params)
    if cat1 = Util.Cache.get_cache(frame.cfg_name, child_digest) do
      %Frame{frame | catalog: cat1}
    else
      cat2 = Catalog.create_from_frame(frame)
      Util.Cache.put_cache(frame.cfg_name, child_digest, cat2)
      %Frame{frame | catalog: cat2}
    end
  end
end