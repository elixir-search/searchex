defmodule Searchex.Util.App do

  @moduledoc false

  @doc """
  Return the application version
  """
  def version do
    appsym = symbol
    appver = fn
        [{_app, _desc, vsn}] -> vsn
        _                    -> "NA"
    end
    Application.loaded_applications
    |> Enum.filter(fn({atom, _, _}) -> atom == appsym end)
    |> appver.()
  end

  @doc """
  Return the application symbol.

  ## Example

      iex> Searchex.Util.App.symbol
      :searchex
  """
  def symbol do
    Application.get_application(__MODULE__)
  end

  @doc """
  Return the application name.

  ## Example

      iex> Searchex.Util.App.name
      "searchex"
  """
  def name do
    Atom.to_string(symbol)
  end
end

