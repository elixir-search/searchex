defmodule Searchex.Util.AppTest do
  use ExUnit.Case, async: true

  doctest Searchex.Util.App

  import Searchex.Util.App

  test "#symbol" do
    assert symbol == :searchex
  end

  test "#name" do
    assert name == "searchex"
  end
end
