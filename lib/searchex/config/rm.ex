defmodule Searchex.Config.Rm do
  @moduledoc false

  import Searchex.Config.Helpers

  def exec(path) do
      cfg_name = name_from_path(path)
    cond do
#      cfg_name_invalid?(cfg_name) -> {:error, cfg_name_invalid_msg(cfg_name)}
#      cfg_missing?(cfg_name)      -> {:error, cfg_missing_msg(cfg_name)}
      true                        -> System.cmd("rm", [cfg_file(cfg_name)]); {:ok}
    end
  end
end