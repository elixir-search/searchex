defmodule Searchex.Command.DocsourceTest do
  use ExUnit.Case

  alias Searchex.Command.Docsource

  describe "error condition" do
    test "missing cfg" do
      frame = Docsource.exec("unknown")
      assert frame.halted
    end
  end
end
