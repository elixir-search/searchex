defmodule Util.Ext.StringTest do
  use ExUnit.Case, async: true

  import Util.Ext.String

  describe "#wordlist" do
    test "simple string" do
      assert wordlist("a b c d") == ~w(a b c d)
    end

    test "with punctuation" do
      assert wordlist("a,b\nc-d") == ~w(a b c d)
    end

    test "with punctuation and spaces" do
      assert wordlist("a,b\n   c-d") == ~w(a b c d)
    end
  end

  describe "#wordcount" do
    test "simple string" do
      assert wordcount("a b c d") == 4
    end
  end

  describe "wordstems" do
    test "basics" do
      assert wordstems("walked singing") == ~w(walk sing)
    end
  end

  describe "empty?" do
    test "with chars" do
      refute empty?("asdf")
    end

    test "without chars" do
      assert empty?("")
    end
  end

  describe "present?" do
    test "with chars" do
      assert present?("asdf")
    end

    test "without chars" do
      refute present?("")
    end
  end
end
