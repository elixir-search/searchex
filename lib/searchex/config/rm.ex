defmodule Searchex.Config.Rm do
  @moduledoc false

  import Searchex.Config.Helpers
  import Util.CfgValidations

  def exec(path) do
    cfg_name = name_from_path(path)
    case check_validations(validation_list(cfg_name)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> remove_cfg(cfg_name)
    end
  end

  # -----

  defp validation_list(cfg_name) do
    [
      cfg_name_invalid?(cfg_name)     ,
      cfg_missing?(cfg_name)          ,
      cfg_dir_absent?
    ]
  end

  defp remove_cfg(cfg_name) do
    System.cmd("rm", [cfg_file(cfg_name)])
    {:ok}
  end
end