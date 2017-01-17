defmodule Searchex.Command.DocsrcTest do
  use ExUnit.Case

  alias Searchex.Command.Docsrc

  describe "error condition" do
    test "missing cfg" do
      frame = Docsrc.exec("unknown")
      assert frame.halted
    end
  end
end
