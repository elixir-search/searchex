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

    describe "#settings" do
      test "map values" do
        assert Searchex.settings.cfgs != nil
        assert Searchex.settings.docs != nil
        assert Searchex.settings.data != nil
      end
    end
end
