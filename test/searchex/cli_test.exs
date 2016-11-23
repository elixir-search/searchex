defmodule Searchex.CliTest do
  use ExUnit.Case, async: true

  doctest Searchex.Cli

  @cmds ~w(cfg_new cfg_fetch cfg_cat cfg_edit cfg_rm cfg_ls build search version help all_commands cfg_commands)

  describe "#main" do
    for cmd <- @cmds do
      @cmd cmd
      test "#main with '#{@cmd}'" do
        assert is_tuple(Searchex.Cli.main([@cmd])) != true
      end
    end  

    test "call main function with no args" do
      kong = Searchex.Cli.main []
      assert is_tuple(kong) != true
    end
  end

  describe "#route" do
    for cmd <- @cmds do
      @cmd cmd
      test "#route with '#{@cmd}'" do
        assert Searchex.Cli.route([@cmd]) != nil
      end
    end  

    test "bad input data" do
      {status, _msg} = Searchex.Cli.route(~w(unknown))
      assert status == :error
    end
  end

  describe "#help" do
    test ":ok return value" do
      {status, _msg} = Searchex.Cli.help()
      assert  status == :ok
    end
  end  

  describe "#version" do
    test ":ok return value" do
      {status, _msg} = Searchex.Cli.version()
      assert  status == :ok
    end
  end
end
