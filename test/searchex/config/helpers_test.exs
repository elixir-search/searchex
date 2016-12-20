defmodule Searchex.Config.CfgHelpersTest do
  use ExUnit.Case, async: true

  alias Searchex.Config.CfgHelpers

  describe "#cfg_file" do
    test "base" do
      result = CfgHelpers.cfg_file("min")
      assert result != nil
    end
  end
end
