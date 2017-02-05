defmodule Reqm.Proxy do
  use Reqm.Module

  @moduledoc false

  #  With this module, you can invoke custom middleware
  #  from the config file.
  #
  #      :request_module:
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
      nil -> halt(frame, "No request module (#{type})")
      mod -> call_if_loaded(frame, mod)
    end
  end
  
  def call_if_loaded(frame, module) do
    cond do
      Code.ensure_loaded?(module) -> module.call(frame, [])
      true                        -> halt(frame, "Request module not found (#{module})")
    end
  end

  defp modmap(frame) do
    Map.merge(defaults(), custom_modules(frame))
  end

  defp custom_modules(frame) do
    frame.params.req_module
    |> Enum.reduce(%{}, fn({k,v}, acc) -> Map.merge(acc, %{k => xatom(v)})end)
  end

  defp xatom(string) do
    "Searchex.Request.#{string}" |> String.to_atom
  end
end
