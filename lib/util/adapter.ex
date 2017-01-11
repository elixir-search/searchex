defmodule Util.Adapter do
  def event_ids(frame, type \\ :null) do
    frame.params.adapter.module.events(frame)
    |> Enum.map(fn(item) -> elem(item, 1) end)
    |> List.flatten
    |> event_filter(type)
    |> Enum.map(fn(item) -> elem(item, 1) end)
  end

  defp event_filter(list, :null), do: list
  defp event_filter(list, type) do
    Enum.filter(list, fn(item) -> elem(item, 0) == type end)
  end
end