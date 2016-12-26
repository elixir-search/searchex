defmodule Util.Ext.FileTest do
  use ExUnit.Case

  alias Util.Ext.File
  
  describe "#ls_r" do
    test "first" do
      results = File.ls_r(".", globs: ~w(md))
      assert is_list(results)
      assert length(results) == 2
    end
  end
end
