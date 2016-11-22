defmodule Searchex.CfgTest do
  use ExUnit.Case, async: true

  doctest Searchex.Cfg

  describe "#new error conditions" do
    test "with invalid file name" do
      {cond, _skip} = Searchex.Cfg.new("bang!")
      assert cond == :error 
    end
    test "another invalid file name" do
      {cond, _skip} = Searchex.Cfg.new("bang.yml")
      assert cond == :error
    end
  end

  describe "#cat error conditions" do
    test "missing file name" do
      {cond, _skip} = Searchex.Cfg.cat("does_not_exist")
      assert cond == :error 
    end
  end

  describe "#edit error conditions" do
    test "edit invalid file name" do
      {cond, _skip} = Searchex.Cfg.edit("non*sense")
      assert cond == :error 
    end
  end

  describe "#test error conditions" do
    test "under construction" do
      {cond, _skip} = Searchex.Cfg.test("under_construction")
      assert cond == :error 
    end
  end

  describe "#rm error conditions" do
    test "missing file" do
      {cond, _skip} = Searchex.Cfg.test("does_not_exist")
      assert cond == :error 
    end
  end

  describe "#ls" do
    test "returns OK" do
      {cond, _skip} = Searchex.Cfg.ls()
      assert cond == :ok
    end
  end
end
