defmodule Searchex.Config.Cat do
  @moduledoc false

  import Searchex.Config.Helpers
  import Util.CfgValidations

  def exec(cfg_snip) do
    case check_validations(validation_list(cfg_snip)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> cfg_contents(cfg_snip)
    end
  end

  # -----

  defp validation_list(cfg_snip) do
    [
       cfg_snip_invalid?(cfg_snip)     ,
       cfg_ambiguous?(cfg_snip)        ,
       cfg_nomatch?(cfg_snip)          ,
    ]
  end

  defp cfg_contents(cfg_snip) do
    {:ok, str} = File.read(cfg_file(cfg_snip))
    {:ok, str}
  end
end