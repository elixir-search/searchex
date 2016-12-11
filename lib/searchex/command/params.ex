defmodule Searchex.Command.Params do

  import Searchex.Config.Helpers

  @moduledoc false

  use Shake.Module

  def exec(cfg_name) do
    call(%Shake.Frame{cfg_name: cfg_name}, [])
  end

  step :validate_cfg_name

  def validate_cfg_name(frame, opts) do
    halt(frame, "#{__MODULE__}: invalid")
  end

  def gen_params(cfg_name) do
    cfg_name
    |> Searchex.Config.cfg_cat
    |> Searchex.Config.Load.to_map
    |> Searchex.Util.Map.atomify_keys
    |> Searchex.Command.Build.Catalog.Params.create_from_cfg
  end
end
