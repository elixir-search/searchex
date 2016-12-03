defmodule Searchex.Command.Catalog do

  @moduledoc false

  use ExMake

  import Searchex.Config.Helpers

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
    tstamp = newest([
      filepath_timestamp(cfg_file(cfg_name))  ,      # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)             # newest timestamp of all doc_dirs
    ])
    [{:ok, tstamp}]
  end

  def chain_action_when_fresh(args = {:load_catalog, cfg_name}, _child_state) do
    DIO.inspect :FRESH_CATALOG, color: "green"
    state = gen_params(cfg_name) |> Searchex.Command.Build.Catalog.Cache.read_catalog
    {:ok, chain_lcl_timestamp(args), state}
  end

  def chain_action_when_stale(args = {:load_catalog, cfg_name}, _child_state) do
    DIO.inspect :STALE_CATALOG, color: "green"
    state = gen_params(cfg_name)
    |> Searchex.Command.Build.Catalog.Scan.create_from_params
    |> Searchex.Command.Build.Catalog.create_from_scan
    |> Searchex.Command.Build.Catalog.Cache.write_catalog
    {:ok, chain_lcl_timestamp(args), state}
  end

  defp gen_params(cfg_name) do
    cfg_name
    |> Searchex.Config.cfg_cat
    |> Searchex.Config.Load.to_map
    |> Searchex.Util.Map.atomify_keys
    |> Searchex.Command.Build.Catalog.Params.create_from_cfg
  end

  def chain_lcl_timestamp({:load_catalog, cfg_name}) do
    cat_file(cfg_name) |> filepath_timestamp
  end
end
