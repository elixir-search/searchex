defmodule Searchex.Command.Index do

  @moduledoc false

  use Shake.Module

  import Searchex.Command.Build.Index

  @doc """
  The API for the module - takes a config name and returns
  a frame with Params, Catalog and Index filled.
  """
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Catalog
  step :generate_index

  # check to see if the index is in the LRU cache, otherwise regenerate
  def generate_index(frame, _opts) do
    catalog      = frame.catalog
    cfg_name     = frame.cfg_name
    child_digest = frame.digests.catalog
    catalog |> create_from_catalog
    col = X.Term.to_atom(cfg_name)
    val = Searchex.Keyword.Supervisor.otp_to_term(col)
    arg = [
      process_tree: Searchex.Keyword.Supervisor,
      collection:   col,
      value:        val
    ]
    X.Cache.put_cache(child_digest, arg)
    frame
  end
end