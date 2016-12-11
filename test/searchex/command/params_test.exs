defmodule Searchex.Command.ParamsTest do
  use ExUnit.Case

  import Searchex.Command.Params
  alias Shake.Frame

  describe "error condition" do
    test "missing cfg" do
      frame = exec("unknown")
      assert frame.halted
    end
  end

  describe "valid config" do
    test "valid configuration" do
      frame = exec("min")
      refute frame.halted
    end
  end

  describe "digest" do
    test "presence" do
      frame = exec("min")
      refute Frame.get_digest(frame, :params) == nil
    end
  end
end
