defmodule Searchex.Request.IndexTest do
  use ExUnit.Case

  alias Searchex.Request.Index

  describe "min collection" do
    test "starts min keyword supervisor" do
      frame = Index.exec("min")
      pid   = Process.whereis(frame.index)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 69
    end

    test "cached index" do
      frame = Index.exec("min")
      pid   = Process.whereis(frame.index)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 69
    end
  end

  describe "multi collection" do
    test "starts multi keyword supervisor" do
      frame = Index.exec("multi")
      pid   = Process.whereis(frame.index)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 286
    end
  end
end
