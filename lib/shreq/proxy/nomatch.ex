defmodule Shreq.Proxy.Nomatch do
  use Shreq.Module

  @moduledoc false

  def call(frame, []) , do: halt(frame, "NO MESSAGE")
  def call(frame, msg), do: halt(frame, msg)
end