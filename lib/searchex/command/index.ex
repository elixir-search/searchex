defmodule Searchex.Command.Index do

  @moduledoc false

  use Shake.Module
  alias Searchex.Command.Build.Index

  @doc """
  The API for the module - takes a config name and returns
  a frame with Params, Catalog and Index filled.
  """
  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Catalog
  step :generate_index

  def generate_index(frame, _opts) do
    child_digest = "idx_#{frame.cfg_name}_" <> Frame.get_digest(frame, :params)
    if map = Util.Cache.get_cache(frame.cfg_name, child_digest) do
      Index.map_to_otp(map, child_digest)
    else
      Index.create_from_frame(frame, child_digest)
      Util.Cache.put_cache(frame.cfg_name, child_digest, Index.otp_to_map(child_digest))
    end
    %Frame{frame | index: Util.Ext.String.to_atom(child_digest)}
  end
end
