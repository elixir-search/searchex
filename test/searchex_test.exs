defmodule SearchexTest do
  use ExUnit.Case, async: true

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
