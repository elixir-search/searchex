defmodule TIO do
  def inspect(item, opts \\ []) do
    DIO.inspect :stdio, item, opts ++ [test: :show]
  end
end