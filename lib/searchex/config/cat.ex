defmodule Searchex.Config.Cat do
  @moduledoc false

  import Searchex.Config.Helpers
  import ExMakeOld, only: [check_validations: 1]

  def exec(path) do
    cfg_name = name_from_path(path)
    case check_validations(validation_list(cfg_name)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> cfg_contents(cfg_name)
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

  defp cfg_contents(cfg_name) do
    {:ok, str} = File.read(cfg_file(cfg_name))
    {:ok, str}
  end
end