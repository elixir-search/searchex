defmodule Searchex.Command.Index do

  @moduledoc false

  use ExMake

  import Searchex.Command.Build.Index

  def exec(cfg_name) do
    chain({:load_index, cfg_name})
  end

  def chain_children({:load_index, cfg_name}) do
    [ Searchex.Command.Catalog.chain({:load_catalog, cfg_name}) ]
  end

  def chain_generate({:load_index, cfg_name}, child_state) do
    [{catalog, child_digest} | _rest] = child_state
    catalog |> create_from_catalog
    col = X.Term.to_atom(cfg_name)
    val = Searchex.Keyword.Supervisor.otp_to_term(col)
    arg = [
      process_tree: Searchex.Keyword.Supervisor,
      collection:   col,
      value:        val
    ]
    ExCache.put_cache(child_digest, arg)
    catalog
  end
end