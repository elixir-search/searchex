defmodule Searchex.Config.Edit do

  @moduledoc false

  import Searchex.Config.Helpers
  import ExMake, only: [check_validations: 1]

  def exec(cfg_name) do
    make_active_dirs()
    case check_validations(validation_list(cfg_name)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> {:ok, cfg_name}
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
end