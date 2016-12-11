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

  describe "#search" do
#    test "single word search" do
#      result = Searchex.Command.search("worklog", "monit")
#      assert result != nil
#    end

#    test "two word search" do
#      result = Searchex.Command.search("worklog", "monit notes")
#      assert result != nil
#    end

#    test "no results" do
#      result = Searchex.Command.search("worklog", "unrecognizedyoyoyo")
#      assert result != nil
#    end

#    test "blank query" do
#      result = Searchex.Command.search("worklog", "")
#      assert result != nil
#    end
  end

  describe "#query" do
#    test "single word search" do
#      result = Searchex.Command.search("worklog", "monit")
#      assert result != nil
#    end
  end

  describe "#clean" do
    test "removing all files" do
      result = Searchex.Command.clean
      assert result != nil
    end
  end
end
