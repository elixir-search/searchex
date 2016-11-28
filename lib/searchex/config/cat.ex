defmodule Searchex.Config.Cat do
  @moduledoc false

  import Searchex.Config.Helpers

  def exec(path) do
    cfg_name = name_from_path(path)
    cond do
      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> cfg_contents(cfg_name)
    end
  end

  # -----

  defp cfg_contents(cfg_name) do
    {:ok, str} = File.read(cfg_file(cfg_name))
    {:ok, str}
  end
end