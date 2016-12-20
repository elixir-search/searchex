defmodule Searchex.Config.Edit do

  @moduledoc false

  import Searchex.Config.CfgValidations

  def exec(cfg_snip) do
    case check_validations(validation_list(cfg_snip)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> {:ok, cfg_snip}
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
end