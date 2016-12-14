defmodule X.GlobalState do

  @moduledoc false

  def get(name) when is_atom(name) do
    case Process.whereis(name) do
      nil -> nil
      _   -> Agent.get(name, &(&1))
    end
  end

  def set(name, val) when is_atom(name) do
    case Process.whereis(name) do
      nil  -> Agent.start(fn -> val end, name: name)
      _pid -> Agent.update(name, fn(_x) -> val end)
    end
  end
end
