defmodule Searchex.CommandTest do
  use ExUnit.Case, async: true

  doctest Searchex.Command

  describe "#index" do
    test "basic operation" do
      result = Searchex.Command.index("test")
      assert result != nil
    end
  end

  describe "#catalog" do
    test "basic operation" do
      result = Searchex.Command.catalog("test")
      assert result != nil
    end
  end

  describe "#search" do
    test "single word search" do
      result = Searchex.Command.search("test", "monit")
      assert result != nil
    end

    test "two word search" do
      result = Searchex.Command.search("test", "monit notes")
      assert result != nil
    end

    test "no results" do
      result = Searchex.Command.search("test", "unrecognizedyoyoyo")
      assert result != nil
    end

    test "blank query" do
      result = Searchex.Command.search("test", "")
      assert result != nil
    end
  end

  describe "#query" do
    test "single word search" do
      result = Searchex.Command.search("test", "monit")
      assert result != nil
    end
  end
end
