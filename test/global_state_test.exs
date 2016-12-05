defmodule GlobalStateTest do
  use ExUnit.Case

  import GlobalState

  test "basic operation" do
    assert get(:bing) == nil
    set(:ping, :pong)
    assert get(:ping) == :pong
    set(:ping, :zing)
    assert get(:ping) == :zing
  end
end
