defmodule Calc2 do
  use ExActor.GenServer, export: CALC2

  @doc "START Calc2"
  defstart start_link do
    Util.Ext.IO.tins({:starting_calc2}, color: "YELLOW")
    initial_state(0)
  end

  @doc "INC Calc2"
  defcast inc(x), state: state, do: new_state(state + x)

  @doc "DEC Calc2"
  defcast dec(x), state: state, do: new_state(state - x)

  @doc "GET Calc2"
  defcall get, state: state, do: reply(state)

  @doc "STOP Calc2"
  defcast stop, do: stop_server(:normal)
end
