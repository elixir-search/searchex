defmodule Searchex.Request.ParamsTest do
  use ExUnit.Case

  alias Searchex.Request.Params
  alias Shake.Frame

  describe "error condition" do
    test "missing cfg" do
      frame = Params.exec("unknown")
      assert frame.halted
    end
  end

  describe "valid config" do
    test "valid configuration" do
      frame = Params.exec("min")
      refute frame.halted
    end
  end

  describe "digest" do
    test "presence" do
      frame = Params.exec("min")
      refute Frame.get_digest(frame, :params) == nil
    end
  end
end
