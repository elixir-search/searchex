defmodule Searchex.Util.Enum do

  @moduledoc false

  def average([]) do
    0
  end

  def average(enumerable) do
    Float.floor(Enum.sum(enumerable) / Enum.count(enumerable))
  end

  def join(enumerable, char \\ " ") do
    enumerable
    |> Enum.filter(fn(x) -> Searchex.Util.String.present?(x) end)
    |> Enum.join(char)
  end
end
