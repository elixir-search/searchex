defmodule Searchex.Command.ParamsTest do
  use ExUnit.Case

  import Searchex.Command.Params

  describe "error condition" do
    test "missing cfg" do
      conn = exec("unknown")
      assert conn.halted
    end
  end
end
