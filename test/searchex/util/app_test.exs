defmodule Searchex.Util.AppTest do
  use ExUnit.Case, async: true

  doctest Searchex.Util.App

  import Searchex.Util.App

  test "#verion" do
    assert version == '0.0.1-alpha.2'
  end

  test "#symbol" do
    assert symbol == :searchex
  end

  test "#name" do
    assert name == "searchex"
  end
end
