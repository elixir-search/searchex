defmodule Searchex.Adapter.Validate do
  def validate_frame(frame, _opts) do
    frame
  end

  def defined_type?(args) do
    err = {:error, "Adapter type not defined"}
    if args[:type], do: {:ok}, else: err
  end

  def type_exists?(type, match) do
    err = {:error, "Adapter type does not exist (#{type})"}
    if match == nil, do: err, else: {:ok}
  end
end