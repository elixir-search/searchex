defmodule Searchex.Command.Docsource do

  @moduledoc false

  use Shake.Module
  alias Shake.Frame

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step :basic_adapter_validation
  step :type_specific_adapter_params
  step :type_specific_adapter_validation
  step :start_adapter
  step :pull_adapter

  def basic_adapter_validation(frame, opts) do
    Searchex.Adapter.validate(frame, opts)
  end

  def type_specific_adapter_params(%Frame{params: params} = frame, _opts) do
    mod     = Searchex.Adapter.adapter_module(frame)
    adapter = Map.merge(params[:adapter], mod.default_params())
    put_in(frame, [:adapter], adapter)
  end

  def type_specific_adapter_validation(frame, opts) do
    adapter_module = Searchex.Adapter.adapter_module(frame)
    adapter_module.validate(frame, opts)
  end

  def start_adapter(frame, _opts) do
    frame
  end

  def pull_adapter(frame, _opts) do
    frame
  end
end
