defmodule Searchex.Command.Params do

  @moduledoc false
#
#  use ExMakeOld
#
#  import Searchex.Config.Helpers
#
#  def exec(cfg_name) do
#    chain({:load_params, cfg_name})
#  end
#
#  def chain_validations({:load_params, cfg_name}) do
#
#    [
#      cfg_name_invalid?(cfg_name),
#      cfg_dir_absent?            ,
#      cfg_missing?(cfg_name)     ,
#      cfg_invalid?(cfg_name)
#    ]
#  end

  # This is non-standard implementation of 'chain-children' at this level, the
  # two dependencies are the cfg file and the document files.  So instead of
  # calling to chained-children, we simply return the newest modification date.
#  def chain_children({:load_params, cfg_name}) do
#    params = gen_params(cfg_name)
#    tstamp = newest([
#      filepath_timestamp(cfg_file(cfg_name))  ,      # timestamp of the cfg file
#      dirlist_timestamp(params.doc_dirs)             # newest timestamp of all doc_dirs
#    ])
#    [{:ok, tstamp}]
#  end
#
#  def chain_action_when_fresh(args = {:load_params, cfg_name}, _child_state) do
#    DIO.inspect :FRESH_CATALOG, color: "green"
#    state = gen_params(cfg_name) |> Searchex.Command.Build.Catalog.Cache.read_catalog
#    {:ok, chain_lcl_timestamp(args), state}
#  end
#
#  def chain_action_when_stale(args = {:load_params, cfg_name}, _child_state) do
#    DIO.inspect :STALE_CATALOG, color: "green"
#    state = gen_params(cfg_name)
#    |> Searchex.Command.Build.Catalog.Filescan.create_from_params
#    |> Searchex.Command.Build.Catalog.create_from_scan
#    |> Searchex.Command.Build.Catalog.Cache.write_catalog
#    {:ok, chain_lcl_timestamp(args), state}
#  end
#
#  # this belongs in another level
#  defp gen_params(cfg_name) do
#    cfg_name
#    |> Searchex.Config.cfg_cat
#    |> Searchex.Config.Load.to_map
#    |> Searchex.Util.Map.atomify_keys
#    |> Searchex.Command.Build.Catalog.Params.create_from_cfg
#  end
#
#  def chain_lcl_timestamp({:load_params, cfg_name}) do
#    cat_file(cfg_name) |> filepath_timestamp
#  end
#
#  defp cfg_filename(cfg_name) do
#    Searchex.settings.cfgs <> "/#{cfg_name}.yml"
#  end
#
#  defp cfg_text(cfg_name) do
#    File.read(cfg_filename(cfg_name))
#  end
#
#  @doc "Return the CFG data as a Map"
#  def to_map({:ok, yaml_data}) do
#    YamlElixir.read_from_string yaml_data, atoms: true
#  end
#
#  def to_map(cfg_name) do
#    {:ok, yaml_data} = Searchex.Config.cfg_cat(cfg_name)
#    YamlElixir.read_from_string yaml_data
#  end
end
