defmodule Searchex.Command.Catalog do
  use ExMake

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    chain({:load_catalog, cfg_name})
  end

  # -----

  @doc false
  def handle_chain({:load_catalog, cfg_name}) do
    DIO.inspect [KONG: cfg_name], color: "RED"
    %{
      validations:       fn -> validations(cfg_name)                   end  ,
      children:          fn -> [newest_child_timestamp(cfg_name)]      end  ,
      lcl_timestamp:     fn -> lcl_timestamp(cfg_name)                 end  ,
      action_when_fresh: fn -> generate_catalog_from_scratch(cfg_name) end  ,
      action_when_stale: fn -> load_catalog_from_cache(cfg_name)       end  ,
    }
  end

  defp validations(cfg_name) do
    [
      cfg_name_invalid?(cfg_name),
      cfg_dir_absent?(cfg_name)  ,
      cfg_missing?(cfg_name)     ,
      cfg_invalid?(cfg_name)
    ]
  end

  defp newest_child_timestamp(cfg_name) do
    params  = gen_params(cfg_name)
    [
      filepath_timestamp(cfg_file(cfg_name))  ,      # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)             # newest timestamp of all doc_dirs
    ]
    |> newest
  end

  defp load_catalog_from_cache(cfg_name) do
    gen_params(cfg_name)
    |> Searchex.Command.Build.Catalog.Cache.read_catalog
  end

  defp generate_catalog_from_scratch(cfg_name) do
    gen_params(cfg_name)
    |> Searchex.Command.Build.Catalog.Scan.create_from_params
    |> Searchex.Command.Build.Catalog.create_from_scan
    |> Searchex.Command.Build.Catalog.Cache.write_catalog
  end

  defp gen_params(cfg_name) do
    cfg_name
    |> Searchex.Config.cfg_cat
    |> Searchex.Config.Load.to_map
    |> Searchex.Util.Map.atomify_keys
    |> Searchex.Command.Build.Catalog.Params.create_from_cfg
  end

  defp lcl_timestamp(cfg_name) do
    cat_file(cfg_name) |> filepath_timestamp
  end
end
