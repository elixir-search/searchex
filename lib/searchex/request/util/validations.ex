defmodule Searchex.Request.Util.Validations do

  import Reqm.Frame
  alias  Searchex.Config.CfgValidations

  @moduledoc false

  def always_true(frame, _opts) do
    frame
  end

  def always_false(frame, _opts) do
    halt(frame, "ALWAYS FALSE VALIDATION")
  end

  def cfg_snip_invalid?(frame, _opts) do
    case CfgValidations.cfg_snip_invalid?(frame.cfg_snip) do
      {:ok}         -> frame
      {:error, msg} -> halt(frame, msg)
    end
  end

  def cfg_nomatch?(frame, _opts) do
    case CfgValidations.cfg_nomatch?(frame.cfg_snip) do
      {:ok}         -> frame
      {:error, msg} -> halt(frame, msg)
    end
  end

  def cfg_ambiguous?(frame, _opts) do
    case CfgValidations.cfg_ambiguous?(frame.cfg_snip) do
      {:ok}         -> frame
      {:error, msg} -> halt(frame, msg)
    end
  end
end
