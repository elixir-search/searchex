defmodule X.TIO do

  @moduledoc false

  def inspect(item, opts \\ []) do
    X.DIO.inspect :stdio, item, opts ++ [test: :show]
  end
end
