defmodule ExMakeTest do
  use ExUnit.Case
  
  describe "#version" do
    test "execution" do
      assert Searchex.version != nil
    end

    test ":ok return value" do
      {status, _msg} = Searchex.version()
      assert  status == :ok
    end
  end
end
