defmodule Searchex.Command.DevRoute do

  # This is temporary, so we can keep the old code working while
  # developing the new Adapter Middleware

  @moduledoc false

  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params and Catalog slots filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step :select_path

  def select_path(frame, opts) do
  Util.Ext.IO.tins("HELLO", color: "CYAN")
    if frame.params.adapter == %{} do
      Searchex.Command.Params.call(frame, opts)
    else
      Searchex.Command.ParamsNew.call(frame, opts)
    end
  end
end