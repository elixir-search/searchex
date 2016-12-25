defmodule Util.Ext.AppTest do
  use ExUnit.Case, async: true

  doctest Util.Ext.App

  import Util.Ext.App

  test "#symbol" do
    assert symbol == :searchex
  end

  test "#name" do
    assert name == "searchex"
  end
end
