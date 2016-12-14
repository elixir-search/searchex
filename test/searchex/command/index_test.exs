defmodule Searchex.Command.IndexTest do
  use ExUnit.Case

  import Searchex.Command.Index

  describe "min collection" do
    test "starts min keyword supervisor" do
      frame = exec("min")
      pid   = Process.whereis(:min)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 69
      assert frame.index == "min"
    end
  end

  describe "multi collection" do
    test "starts multi keyword supervisor" do
      frame = exec("multi")
      pid   = Process.whereis(:multi)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 248
      assert frame.index == "multi"
    end
  end
end
