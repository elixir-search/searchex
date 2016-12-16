defmodule Searchex.Command.Build do

  @moduledoc false

  use Shake.Module

  def exec(cfg_name) do
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Index

end
