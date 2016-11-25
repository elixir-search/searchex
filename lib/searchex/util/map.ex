defmodule Searchex.Util.Map do

  @moduledoc false

  def atomify_keys(map) do
    Enum.reduce(map, %{}, fn({k,v}, acc) -> Map.merge(acc, %{atomify(k) => v}) end)
  end

  defp atomify(ele) do
    if is_binary(ele) do
      String.to_atom(ele)
    else
      ele
    end
  end

  def deep_merge(left, right) do
    Map.merge(left, right, &deep_resolve/3)
  end

  # Key exists in both maps, and both values are maps as well.
  # These can be merged recursively.
  defp deep_resolve(_key, left = %{}, right = %{}) do
    deep_merge(left, right)
  end

  # Key exists in both maps, but at least one of the values is
  # NOT a map. We fall back to standard merge behavior, preferring
  # the value on the right.
  defp deep_resolve(_key, _left, right) do
    right
  end
end
