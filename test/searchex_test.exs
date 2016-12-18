defmodule SearchexTest do
  use ExUnit.Case, async: true

    describe "#version" do
      test "execution" do
        assert SearchexOld.version != nil
      end

      test ":ok return value" do
        {status, _msg} = SearchexOld.version()
        assert  status == :ok
      end
    end

    describe "#base_dir" do
      test "value" do
        refute Searchex.base_dir == nil
      end
    end
end
