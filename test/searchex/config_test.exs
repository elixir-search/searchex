defmodule Searchex.ConfigTest do
  use ExUnit.Case, async: true

  doctest Searchex.Config

  describe "#new" do
    @tag :skip
    test "with invalid path name" do
      {cond, _skip} = Searchex.Config.new("bang!")
      assert cond == :error
    end

    @tag :skip
    test "with valid path name" do
      {cond, _skip} = Searchex.Config.new("bang.yml")
      assert cond == :error
    end

    @tag :pending
    test "with relative path"

    @tag :pending
    test "with current relative path"
  end

  describe "#sample_fetch" do
    test "invalid config name" do
      {cond, _skip} = Searchex.Config.sample_fetch("non*sense")
      assert cond == :error
    end
  end

  describe "#cat" do
    test "missing file name" do
      {cond, _skip} = Searchex.Config.cat("does_not_exist")
      assert cond == :error 
    end
  end

  describe "#edit" do
    test "invalid file name" do
      {cond, _skip} = Searchex.Config.edit("non*sense")
      assert cond == :error
    end
  end

  describe "#ls" do
    test "with valid input" do
      {cond, _skip} = Searchex.Config.ls_old()
      assert cond == :ok
    end
  end

  describe "#rm" do
    test "with invalid cfg name" do
      {cond, _skip} = Searchex.Config.rm("nonexist")
      assert cond == :error
    end
  end
end
