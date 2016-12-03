defmodule Searchex.Command.Build.Catalog.ParamsTest do

  use ExUnit.Case, async: true

  import Searchex.Command.Build.Catalog.Params

  describe "#regify" do
    test "with a string" do
      assert regify("---") == ~r/---/
    end

    test "with a regex" do
      assert regify(~r/---/) == ~r/---/
    end
  end
end