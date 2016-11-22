defmodule Searchex.Util.IO do

  @moduledoc false

  def inspect(ele, arg \\ []) do
    case Mix.env do
      :dev  -> IO.inspect ele, arg
      :prod -> IO.inspect ele, arg
      :test -> :OK
      _     -> :OK
    end
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
