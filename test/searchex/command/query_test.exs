defmodule Searchex.Command.QueryTest do
  use ExUnit.Case

  import Searchex.Command.Query

  describe "multi collection" do
    test "doc count" do
      frame = exec("multi", "monit")
      assert length(frame.results) == 4
    end
  end

  describe "tweets collection" do
    test "doc count" do
      frame = exec("tweets", "bing")
      assert length(frame.results) == 0
    end
  end
end
