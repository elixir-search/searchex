defmodule Searchex.Command.Catalog do
  use ExMake

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    handle_chain({:load_catalog, cfg_name})
  end

  def handle_chain({:load_catalog, cfg_name}) do
    %{
      validations: &(validations(cfg_name)) ,
      children:    &(child_timestamps(cfg_name)),
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



  def handle_chain({:load_catalog, cfg_name}) do
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name) }
      cfg_dir_absent?(cfg_name)   -> {:error, cfg_dir_absent_msg(cfg_name)   }
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)      }
      cfg_invalid?(cfg_name)      -> {:error, cfg_invalid_msg(cfg_name)      }
      true                        -> load_catalog(cfg_name)
    end
  end

  # -----

  defp load_catalog(cfg_name) do
    if stale?(cfg_name) do
      {:ok, timestamp_now, generate_catalog_from_scratch(cfg_name)}
    else
      {:ok, filepath_timestamp(cfg_file(cfg_name)), load_catalog_from_cache(cfg_name)}
    end
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

  defp stale?(cfg_name) do
    if File.exists?(cat_file(cfg_name)) do
      is? newest_parent_timestamp(cfg_name), newer_than: lcl_timestamp(cfg_name)
    else
      true
    end
  end

  def newest_parent_timestamp(cfg_name) do
    params  = gen_params(cfg_name)
    [
      filepath_timestamp(cfg_file(cfg_name))  ,      # timestamp of the cfg file
      dirlist_timestamp(params.doc_dirs)             # newest timestamp of all doc_dirs
    ]
    |> newest
  end

  def lcl_timestamp(cfg_name) do
    cat_file(cfg_name) |> filepath_timestamp
  end
end
