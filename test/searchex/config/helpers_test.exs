defmodule Searchex.Config.HelpersTest do
  use ExUnit.Case, async: true

  alias Searchex.Config.Helpers

  describe "#cfg_file" do
    test "base" do
      result = Helpers.cfg_file("min")
      assert result != nil
    end
  end
end
