defmodule Util.Ext.Enum do

  @moduledoc false

  def average([]) do
    0
  end

  def average(enumerable) do
    Float.floor(Enum.sum(enumerable) / Enum.count(enumerable))
  end

  def join(enumerable, char \\ " ") do
    enumerable
    |> Enum.filter(fn(x) -> Util.Ext.String.present?(x) end)
    |> Enum.join(char)
  end
end
