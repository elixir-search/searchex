defmodule Searchex.Command.Catalog do

  @moduledoc false

  use ExMake

  import Searchex.Config.Helpers
  import TimeStamp

  def exec(cfg_name) do
    chain({:load_catalog, cfg_name})
  end

  def chain_validations({:load_catalog, cfg_name}) do
    [
      cfg_name_invalid?(cfg_name),
      cfg_dir_absent?            ,
      cfg_missing?(cfg_name)     ,
      cfg_invalid?(cfg_name)
    ]
  end

  # This is non-standard implementation of 'chain-children' at this level, the
  # two dependencies are the cfg file and the document files.  So instead of
  # calling to chained-children, we simply return the newest modification date.
  def chain_children({:load_catalog, cfg_name}) do
    params = gen_params(cfg_name)
    term = [
      filepath_timestamp(cfg_file(cfg_name))  , # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)        # newest timestamp of all doc_dirs
    ] |> newest
    [{:ok, term_digest(term, cfg_name)}]
  end

  def chain_generate({:load_catalog, cfg_name}, _child_state) do
    gen_params(cfg_name)
    |> Searchex.Command.Build.Catalog.create_from_params
  end

  # this belongs in another level...
  defp gen_params(cfg_name) do
    cfg_name
    |> Searchex.Config.cfg_cat
    |> Searchex.Config.Load.to_map
    |> Searchex.Util.Map.atomify_keys
    |> Searchex.Command.Build.Catalog.Params.create_from_cfg
  end
end
