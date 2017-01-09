defmodule Searchex.Adapter do

  def validate(frame, _opts) do
    import Shake.Frame
    cond do
      adapter_missing?(frame)        ->
        halt(frame, "No adapter specified for config (#{frame.collection})")
      adapter_type_missing?(frame)   ->
        halt(frame, "Adapter type not specified")
      adapter_type_unrecognized?(frame) ->
        type = adapter_type(frame)
        halt(frame, "Unrecognized adapter (#{type})")
      true -> frame
    end
  end

  # -----

  def adapter_process(frame) do
    adapter_type(frame) <> "_#{frame.params.collection}"
  end

  def adapter_type(frame) do
    get_in(frame, [:params, :adapter, :type])
  end

  def adapter_module(frame) do
    type = "Searchex.Adapter.Type." <> String.capitalize(frame.params.adapter.type)
           |> Regex.compile!
    :application.get_key(:searchex, :module)
    |> Enum.map(&(Atom.to_string(&1)))
    |> Enum.find(&(Regex.match?(type, &1)))
    |> Util.Ext.IO.tins(color: "RED")
  end

  # -----

  defp adapter_missing?(frame) do
    get_in(frame, [:params, :adapter]) == nil
  end

  defp adapter_type_missing?(frame) do
    adapter_type(frame) == nil
  end

  defp adapter_type_unrecognized?(frame) do
    adapter_module(frame) == nil
  end
end