defmodule Util.Ext.FileTest do
  use ExUnit.Case

  alias Util.Ext.File
  
  describe "#ls_r" do
    test "limit with options" do
      opts = %{depth: 2, types: ~w(md), skips: ~w(^\\..+ ^\_ deps)}
      results = File.ls_r ".", opts
      assert length(results) == 3
    end
  end
end
