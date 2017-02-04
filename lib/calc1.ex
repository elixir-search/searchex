defmodule Calc1 do

  @moduledoc false

  use GenServer

  def start_link do
    Util.Ext.IO.tins({:starting_calc1}, color: "YELLOW")
    GenServer.start_link(__MODULE__, 0, name: CALC)
  end

  def inc(x) do
    GenServer.call(CALC, {:inc, x})
  end

  def dec(x) do
    GenServer.call(CALC, {:dec, x})
  end

  def get do
    GenServer.call(CALC, {:get})
  end

  # -----

  def handle_call({:inc, x}, _from, state) do
    {:reply, state + x, state + x}
  end

  def handle_call({:dec, x}, _from, state) do
    {:reply, state - x, state - x}
  end

  def handle_call({:get}, _from, state) do
    {:reply, state, state}
  end
end
