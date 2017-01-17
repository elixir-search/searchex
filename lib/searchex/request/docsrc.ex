defmodule Searchex.Request.Docsrc do

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
  step :type_specific_adapter_shake
  step :start_adapter
  step :generate_digest

  def basic_adapter_validation(frame, opts) do
    Searchex.Adapter.validate(frame, opts)
  end

  def type_specific_adapter_params(%Frame{params: params} = frame, _opts) do
    adapter_mod      = Searchex.Adapter.adapter_module(frame)
    adapter_settings = Map.merge(adapter_mod.default_settings(), params.adapter)
    put_in(frame, [Access.key(:params, nil), Access.key(:adapter, nil)], adapter_settings)
  end

  def type_specific_adapter_shake(frame, opts) do
    frame.params.adapter.module.shake(frame, opts)
  end

  # TODO: finish this!
  def start_adapter(frame, _opts) do
    frame
  end

  def generate_digest(%Frame{params: params} = frame, _opts) do
    digest = params.adapter.module.cursor(frame)
    set_digest(frame, :docsource, digest)
  end
end
