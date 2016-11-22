defmodule Searchex.CmdTest do
  use ExUnit.Case, async: true

  doctest Searchex.Cfg

  describe "#index" do
    test "basic operation" do
      result = Searchex.Cmd.index("test")
      assert result != nil
    end
  end

  describe "#catalog" do
    test "basic operation" do
      result = Searchex.Cmd.catalog("test")
      assert result != nil
    end
  end

  describe "#search" do
    test "single word search" do
      result = Searchex.Cmd.search("test", "monit")
      assert result != nil
    end

    test "two word search" do
      result = Searchex.Cmd.search("test", "monit notes")
      assert result != nil
    end

    test "no results" do
      result = Searchex.Cmd.search("test", "unrecognizedyoyoyo")
      assert result != nil
    end

    test "blank query" do
      result = Searchex.Cmd.search("test", "")
      assert result != nil
    end
  end
end
