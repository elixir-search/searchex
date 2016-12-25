defmodule Searchex.Config.LsTest do
  use ExUnit.Case, async: true

  describe "#exec" do
    test "base" do
      result = Searchex.Config.Ls.exec
      assert result != nil
    end
  end
end
