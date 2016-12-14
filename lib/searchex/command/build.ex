defmodule Searchex.Command.Build do

  @moduledoc false

  use Shake.Module

  def exec(cfg_name) do
    Searchex.Command.Index.exec(cfg_name)
    call(%Frame{cfg_name: cfg_name}, [])
  end

  step Searchex.Command.Index

end
