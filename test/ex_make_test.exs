defmodule TestMod1 do
  use ExMake
end

defmodule TestMod2 do
  use ExMake
  def chain_validations(_args), do: [ {:error, "FAIL"} ]
end

defmodule TestMod3 do
  use ExMake
  def chain_validations(_args), do: [ {:error, "FAIL"}, {:error, "WHALE"} ]
end

defmodule TestMod4 do
  use ExMake
  def chain_validations(_args), do: [ {:error, "FAIL"}, {:ok}, {:error, "WHALE"} ]
end

defmodule TestMod5 do
  use ExMake
  def chain_validations(_args), do: [ fn() -> {:error, "FUNC"} end ]
end

defmodule TestMod6 do
  use ExMake
  def chain_validations(args), do: [ fn() -> {:error, args} end ]
end

defmodule TestMod7 do
  use ExMake
  def chain_validations(args), do: [ do_it(args) ]
  def do_it(args), do: {:error, args}
end

defmodule ExMakeTest do
  use ExUnit.Case

  describe "#check_validations" do
    test "with empty list" do
      assert  ExMake.check_validations([]) == {:ok}
    end
  end

  describe "#chain" do
    test "with empty module" do
      {state, _digest} = TestMod1.chain
      assert state == :notimpl
    end

    test "with failing validation" do
      assert TestMod2.chain({:test}) == {:error, ["FAIL"]}
      assert TestMod3.chain({:test}) == {:error, ["FAIL", "WHALE"]}
      assert TestMod4.chain({:test}) == {:error, ["FAIL", "WHALE"]}
      assert TestMod5.chain({:test}) == {:error, ["FUNC"]}
      assert TestMod6.chain({:test}) == {:error, [{:test}]}
      assert TestMod7.chain({:test}) == {:error, [{:test}]}
    end
  end
end
