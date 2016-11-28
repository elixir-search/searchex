defmodule Searchex.ConfigTest do
  use ExUnit.Case, async: true

  doctest Searchex.Config

  describe "#cfg_new" do
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

  describe "#cfg_fetch" do
    test "invalid config name" do
      {cond, _skip} = Searchex.Config.cfg_fetch("non*sense")
      assert cond == :error
    end
  end

  describe "#cfg_cat" do
    test "missing file name" do
      {cond, _skip} = Searchex.Config.cfg_cat("does_not_exist")
      assert cond == :error 
    end
  end

  describe "#cfg_edit" do
    test "invalid file name" do
      {cond, _skip} = Searchex.Config.cfg_edit("non*sense")
      assert cond == :error
    end
  end

  describe "#cfg_ls" do
    test "with valid input" do
      {cond, _skip} = Searchex.Config.cfg_ls()
      assert cond == :ok
    end
  end

  describe "#cfg_rm" do
    test "with invalid cfg name" do
      {cond, _skip} = Searchex.Config.cfg_rm("nonexist")
      assert cond == :error
    end
  end
end
