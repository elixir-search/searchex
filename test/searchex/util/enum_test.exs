defmodule Searchex.Util.EnumTest do
  use ExUnit.Case, async: true

  import Searchex.Util.Enum

  describe "#average" do
    test "empty list" do
      assert average([]) == 0
    end

    test "one element list" do
      assert average([2]) == 2
    end

    test "multi-element list" do
      assert average([2,2,2,2,2]) == 2
    end
  end
end
