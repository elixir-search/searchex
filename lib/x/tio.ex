defmodule X.TIO do
  def inspect(item, opts \\ []) do
    X.DIO.inspect :stdio, item, opts ++ [test: :show]
  end
end