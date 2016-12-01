defmodule Searchex.Command.Index do

  use ExMake

  import Searchex.Config.Helpers

  def exec(cfg_name) do
    DIO.inspect "DING EXEC", color: "RED"
    handle_chain({:load_index, cfg_name})
  end

  def handle_chain({:load_index, cfg_name}) do
    DIO.inspect "DING HANDLE", color: "RED"
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name) }
      cfg_dir_absent?(cfg_name)   -> {:error, cfg_dir_absent_msg(cfg_name)   }
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)      }
      cfg_invalid?(cfg_name)      -> {:error, cfg_invalid_msg(cfg_name)      }
      true                        -> load_index(cfg_name)
    end
  end

  # -----

  defp load_index(cfg_name) do
    {:ok, timestamp, catalog} = DIO.inspect(Searchex.Command.Catalog.exec(cfg_name), color: "CYAN")
    DIO.inspect timestamp, color: "RED"  , label: "AAAAA"
    DIO.inspect catalog  , color: "GREEN", label: "YYYYY", limit: 8
    if stale?(cfg_name) do
      generate_index_from_scratch(cfg_name)
      {:ok, catalog}
    else
      load_index_from_cache(cfg_name)
    end
  end

  defp generate_index_from_scratch(_cfg_name) do
      {:ok}Searchex.Command.Build.Index.read_or_generate(catalog)
    "OK"
  end

  defp load_index_from_cache(_cfg_name) do
    "OK"
  end

  defp stale?(_cfg_name) do
    false
  end
end