defmodule Searchex.CommandTest do
  use ExUnit.Case

  describe "#catalog" do
    test "basic operation" do
      result = Searchex.Command.catalog("worklog")
      assert result != nil
    end
  end

  describe "#index" do
    test "basic operation" do
      result = Searchex.Command.index("worklog")
      assert result != nil
    end
  end

  describe "#query" do
    test "single word search" do
      result = Searchex.Command.query("worklog", "monit")
      assert result != nil
    end

    test "two word search" do
      result = Searchex.Command.query("worklog", "monit notes")
      assert result != nil
    end

    test "no results" do
      result = Searchex.Command.query("worklog", "unrecognizedyoyoyo")
      assert result != nil
    end

    test "blank query" do
      result = Searchex.Command.query("worklog", "")
      assert result != nil
    end
  end
end
