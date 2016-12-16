defmodule Searchex.Command.QueryTest do
  use ExUnit.Case

  alias Searchex.Command.Query

  describe "min collection" do
    test "doc count" do
      frame = Query.exec("min", "jan")
      pid   = Process.whereis(frame.index)
      assert is_pid(pid)
      assert Supervisor.count_children(pid).active == 69
      assert length(frame.scores) == 7
    end

    test "another doc count" do
      frame = Query.exec("min", "feb")
      assert length(frame.scores) == 1
    end
  end

  describe "tweets collection" do
    test "doc count" do
      frame = Query.exec("tweets", "bing")
      assert length(frame.scores) == 0
    end
  end
end
