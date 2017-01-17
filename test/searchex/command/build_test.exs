defmodule Searchex.Request.BuildTest do
  use ExUnit.Case

  import Searchex.Request.Build

  describe "min build" do
    test "runs min build" do
      frame = exec("min")
      refute frame == nil
    end
  end
end
