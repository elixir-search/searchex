defmodule Shake.Proxy.Nomatch do
  use Shake.Module

  @moduledoc false

  def call(frame, []) , do: halt(frame, "NO MESSAGE")
  def call(frame, msg), do: halt(frame, msg)
end