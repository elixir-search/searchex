defmodule Searchex.Config.Rm do
  @moduledoc false

  import Searchex.Config.CfgHelpers
  import Searchex.Config.CfgValidations

  def exec(cfg_snip) do
    case check_validations(validation_list(cfg_snip)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> clean_up(cfg_snip)
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

  defp clean_up(cfg_snip) do
    Searchex.Command.CmdHelpers
    frame = Searchex.Command.params(cfg_snip)
    System.cmd("rm", ["-f", CmdHelpers.cache_file(frame)])
    System.cmd("rm", ["-f", CmdHelpers.cfg_file(frame)])
    {:ok}
  end
end