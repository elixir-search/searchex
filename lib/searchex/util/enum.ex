defmodule Searchex.Util.Enum do

  @moduledoc false

  def average([]) do
    0
  end

  def average(enumerable) do
    Float.floor(Enum.sum(enumerable) / Enum.count(enumerable))
  end
end
