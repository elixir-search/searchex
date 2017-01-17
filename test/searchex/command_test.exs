defmodule Searchex.RequestTest do
  use ExUnit.Case

  describe "#catalog" do
    test "basic operation" do
      result = Searchex.Request.catalog("worklog")
      assert result != nil
    end
  end

  describe "#index" do
    test "basic operation" do
      result = Searchex.Request.index("worklog")
      assert result != nil
    end
  end

  describe "#query" do
    test "single word search" do
      result = Searchex.Request.query("worklog", "monit")
      assert result != nil
    end

    test "two word search" do
      result = Searchex.Request.query("worklog", "monit notes")
      assert result != nil
    end

    test "no results" do
      result = Searchex.Request.query("worklog", "unrecognizedyoyoyo")
      assert result != nil
    end

    test "blank query" do
      result = Searchex.Request.query("worklog", "")
      assert result != nil
    end
  end
end
