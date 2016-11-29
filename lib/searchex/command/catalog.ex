defmodule Searchex.Command.Catalog do
  use ExMake

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    handle_chain(cfg_name)
  end

  def handle_chain({:load_catalog, cfg_name}) do
    cond do
      cfg_name_invalid?(cfg_name) -> cfg_name_invalid_msg(cfg_name)
      cfg_dir_absent?(cfg_name)   -> cfg_dir_absent_msg(cfg_name)
      cfg_missing?(cfg_name)      -> cfg_missing_msg(cfg_name)
      cfg_invalid?(cfg_name)      -> cfg_invalid_msg(cfg_name)
      true                        -> load_catalog(cfg_name)
    end
  end

  # -----

  defp load_catalog(cfg_name) do
    if stale?(cfg_name) do
      generate_catalog_from_scratch(cfg_name)
    else
      load_catalog_from_cache(cfg_name)
    end
  end

  defp load_catalog_from_cache(cfg_name) do
    "OK #{cfg_name}"
  end

  defp generate_catalog_from_scratch(cfg_name) do
      Searchex.Command.Build.Catalog.assemble(cfg_name)
  end

  defp stale?(cfg_name) do
    lcl_timestamp(cfg_name)
  end

  def lcl_timestamp(cfg_name) do
    cat_file(cfg_name) |> ExMake.filepath_timestamp
  end
end