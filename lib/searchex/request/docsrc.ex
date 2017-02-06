defmodule Searchex.Request.Docsrc do

  @moduledoc false

  use Reqm.Module
  alias Reqm.Frame

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  step :basic_adapter_validation
  step :adapter_params_setup
  step :adapter_reqm_call
  step :start_adapter
  step :generate_digest

  def basic_adapter_validation(frame, opts) do
    Searchex.Adapter.validate(frame, opts)
  end

  # 1) construct the adapter module atom from the "adapter.type" string
  # 2) merge the adapter params with defaults
  def adapter_params_setup(%Frame{params: params} = frame, _opts) do
    with adapter_module   <- Searchex.Adapter.adapter_module(frame),
         default_settings <- adapter_module.default_settings(),
         adapter_settings <- Map.merge(default_settings, params.adapter),
         param_key        <- Access.key(:params, nil),
         access_key       <- Access.key(:adapter, nil),
         do: put_in(frame, [param_key, access_key], adapter_settings)
  end

  # call reqm middleware for the adapter
  # the reqm step can 1) perform validations and/or 2) update the frame
  def adapter_reqm_call(frame, opts) do
    frame.params.adapter.module.reqm_call(frame, opts)
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
