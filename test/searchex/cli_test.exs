defmodule Searchex.CliTest do
  use ExUnit.Case, async: true

  doctest Searchex.Cli

  @cmd_list Searchex.Cli.command_list_for_testing

  describe "#main" do
    for cmd <- @cmd_list do
      @cmd cmd
      test "with '#{@cmd}'" do
        var = is_tuple(Searchex.Cli.main([@cmd]))
        IO.inspect {@cmd, var, Searchex.Cli.main([@cmd])}
        refute var == true
      end
    end  

    test "call main function with no args" do
      val = Searchex.Cli.main []
      refute is_tuple(val)
    end
  end

  describe "#route" do
    for cmd <- @cmd_list do
      @cmd cmd
      test "with '#{@cmd}'" do
        assert Searchex.Cli.route([@cmd]) != nil
      end
    end

    test "bad input data" do
      {status, _msg} = Searchex.Cli.route(~w(unknown))
      assert status == :error
    end
  end
end
