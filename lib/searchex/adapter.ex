defmodule Searchex.Adapter do

  def validate(frame, _opts) do
    import Reqm.Frame
    cond do
      adapter_missing?(frame)           ->
        halt(frame, "No adapter specified for config (#{frame.cfg_name})")
      adapter_type_missing?(frame)      ->
        halt(frame, "Adapter type not specified (#{frame.cfg_name})")
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
    # an abomination - really feel let down here...
    get_in(Map.from_struct(frame), [:params, Access.key(:adapter, nil), Access.key(:type, nil)])
  end

  def adapter_module(frame) do
    type = "Searchex.Adapter.Type." <> frame.params.adapter.type
           |> Regex.compile!
    :application.get_key(:searchex, :modules)
    |> elem(1)
    |> Enum.map(&(Atom.to_string(&1)))
    |> Enum.find(&(Regex.match?(type, &1)))
    |> Util.Ext.String.to_atom
  end

  # -----

  defp adapter_missing?(frame) do
    # TODO: fix this awful code - caused because Structs don't implment Access...
    get_in(frame, [Access.key(:params, nil), Access.key(:adapter, nil)]) == nil
  end

  defp adapter_type_missing?(frame) do
    adapter_type(frame) == nil
  end

  defp adapter_type_unrecognized?(frame) do
    adapter_module(frame) == nil
  end
end
