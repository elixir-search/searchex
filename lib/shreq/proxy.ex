defmodule Shake.Proxy do
  use Shake.Module

  @moduledoc false

  #  With this module, you can invoke custom middleware
  #  from the config file.
  #
  #      :command:
  #        :results: "MyIndex"
  #
  #  Where "MyIndex" is mapped to `Searchex.Request.MyIndex`.

  def call(frame, []), do: halt(frame, "PROXY EXIT")
  def call(frame, module) do
    summon(frame, module)
  end

  # -----

  defp defaults do
    %{
      :results => Searchex.Request.Results ,
      :query   => Searchex.Request.Query   ,
      :index   => Searchex.Request.Index   ,
      :catalog => Searchex.Request.Catalog ,
      :params  => Searchex.Request.Params  ,
      :docsrc  => Searchex.Request.Docsrc
    }
  end

  def summon(frame, type) do
    case modmap(frame)[type] do
      nil -> halt(frame, "No command module (#{type})")
      mod -> call_if_loaded(frame, mod)
    end
  end
  
  def call_if_loaded(frame, module) do
    cond do
      Code.ensure_loaded?(module) -> module.call(frame, [])
      true                        -> halt(frame, "Module not found (#{module})")
    end
  end

  defp modmap(frame) do
    Map.merge(defaults(), frame.params.command)
  end
end
