defmodule Util.Ext.FileTest do
  use ExUnit.Case

  alias Util.Ext.File

  describe "#ls_r" do
    test "limit with options" do
      opts = %{file_depth: 2, file_types: ~w(md), file_skips: ~w(^\\..+ ^\_ deps)}
      results = File.ls_r ".", opts
      assert length(results) == 4
    end
  end
end
