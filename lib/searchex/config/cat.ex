defmodule Searchex.Config.Cat do
  @moduledoc false

  import Searchex.Config.Helpers
  import Util.CfgValidations

  def exec(cfg_name) do
    case check_validations(validation_list(cfg_name)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> cfg_contents(cfg_name)
    end
  end

  # -----

  defp validation_list(cfg_name) do
    [
       cfg_name_invalid?(cfg_name)     ,
       cfg_ambiguous?(cfg_name)        ,
       cfg_nomatch?(cfg_name)          ,
    ]
  end

  defp cfg_contents(cfg_name) do
    {:ok, str} = File.read(cfg_file(cfg_name))
    {:ok, str}
  end
end