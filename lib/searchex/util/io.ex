defmodule Searchex.Util.IO do

  @moduledoc false

  def inspect(ele, arg \\ []) do
    new_ele = case Keyword.fetch(arg, :label) do
      {:ok, label} -> {label, ele}
      _            -> ele
    end
    case Mix.env do
      :dev  -> IO.inspect new_ele, arg
      :prod -> IO.inspect new_ele, arg
      :test -> :OK
      _     -> :OK
    end
    ele
  end

  def label(ele, label) do
    IO.inspect label
    ele
  end

  def puts(string) do
    case Mix.env do
      :dev  -> IO.puts string
      :prod -> IO.puts string
      :test -> :OK
      _     -> :OK
    end
  end
end
