defmodule Util.Ext.IO do

  @moduledoc false
  # An IO module with support for debugging

  @doc """
  `dins` = 'Debug INSpect'

  Has the same interface as `IO.inspect` with additional options.

  - color - one of `:red`, `:green`, `:blue`, `:yellow`, `:cyan`, `:magenta`, `:white`

  - label - just like the label option in Elixir 1.4 - useful within pipes

  - length: 0 - print the label but not the item

  - display: `fn(item) -> "OUTPUT" end` custom display function

  - test: :show - show output in `:test` mode

  Note: by default, output is generated in `:dev` and `:prod` but not `:test`.
  """
  def dins(item, opts \\ []) do
    dins :stdio, item, opts
  end

  @doc """
  Inspect `item` according to the given options using the IO `device`.
  """
  def dins(device, item, opts) when is_list(opts) do
    label    = if lbl = opts[:label], do: [Util.Ext.String.to_chardata(lbl), ": "], else: []
    alt_opts = struct(Inspect.Opts, opts)
    new_item = display(item, opts)
    chardata = Inspect.Algebra.format(Inspect.Algebra.to_doc(new_item, alt_opts), alt_opts.width)
    [label, chardata, reset] = modify([label, chardata], opts)
    unless label == "" && chardata == "", do: IO.puts(device, [label, chardata, reset])
    item 
  end

  @doc """
  `tins` = 'Test INSpect'
  """
  def tins(item, opts \\ []) do
    dins :stdio, item, opts ++ [test: :show]
  end

  @doc """
  alias for dins
  """
  def inspect(item, opts \\ []) do
    dins :stdio, item, opts
  end

  @doc """
  Generate output in `:dev` and `:prod` modes, but not in `:test`.

  If you want output in test mode, fall back to `IO.puts`.
  """
  def puts(string) do
    case Mix.env do
      :test -> :OK
      _     -> IO.puts string
    end
  end

  @doc """
  Returns the element type.
  """
  def type(item) when is_atom(item)      , do: :atom
  def type(item) when is_binary(item)    , do: :binary
  def type(item) when is_bitstring(item) , do: :bitstring
  def type(item) when is_boolean(item)   , do: :boolean
  def type(item) when is_float(item)     , do: :float
  def type(item) when is_function(item)  , do: :function
  def type(item) when is_integer(item)   , do: :integer
  def type(item) when is_list(item)      , do: :list
  def type(item) when is_map(item)       , do: :map
  def type(item) when is_pid(item)       , do: :pid
  def type(item) when is_port(item)      , do: :port
  def type(item) when is_reference(item) , do: :reference
  def type(item) when is_tuple(item)     , do: :tuple
  def type(_ele) when true               , do: :unknown

  # ----------------------------------------------------------------

  # support for custom display function
  defp display(item, opts) do
    case opts[:display] do
      nil                       -> item
      fun when is_function(fun) -> fun.(item)
      _                         -> item
    end
  end

  # pipeline for the IO.inspect extensions
  defp modify([label, chardata], options) do
    {[label, chardata], options}
    |> lengthize
    |> colorize
    |> envize
    |> package_for_output
  end

  # package for printing by IO.puts - add ANSI reset
  defp package_for_output({[label, chardata], _options}) do
    [label, chardata, IO.ANSI.reset]
  end

  # don't print the item if the :length option is zero
  defp lengthize({[label, chardata], options}) do
    case options[:length] == 0 do
      true   -> {[label, ""], options}
      _      -> {[label, chardata], options}
    end
  end

  # don't print anything in test mode by default - can be overridden using `test: :show`
  defp envize({[label, chardata], options}) do
    default = [dev: :show, prod: :show, test: :hide, eval: :show]
    modlist = Enum.reduce options, default, fn({k, v}, acc) ->
      if Enum.member?([:test, :prod, :dev], k), do: Keyword.merge(acc, [{k, Util.Ext.String.to_atom(v)}]), else: acc end
    case Keyword.get(modlist, Mix.env) do
      :hide  -> {[""   , ""      ], options}
      _      -> {[label, chardata], options}
    end
  end

  # if necessary, add ANSI color codes
  defp colorize({[label, chardata], options}) do 
    case options[:color] do
      nil   -> {[label, chardata], options}
      color -> {colorcode([label, chardata], String.downcase(color)), options}
    end
  end

  # return strings with ANSI color code
  defp colorcode([label, chardata], color) do
    unless valid_color?(color), do: raise("Unrecognized color #{color}")
    acolor = Util.Ext.String.to_atom(color)
    ansi   = ansicode(acolor)
    [Enum.join([ansi, label]), Enum.join([ansi, chardata])]
  end

  # lookup ANSI color code
  defp ansicode(color) do
    case color do
      :red     -> IO.ANSI.red
      :green   -> IO.ANSI.green
      :blue    -> IO.ANSI.blue
      :yellow  -> IO.ANSI.yellow
      :cyan    -> IO.ANSI.cyan
      :magenta -> IO.ANSI.magenta
      :white   -> IO.ANSI.white
    end
  end

  # list of valid color names
  defp valid_colors do
    words = ~w(red green blue yellow cyan magenta white)
    atoms = Enum.map(words, fn(x) -> Util.Ext.String.to_atom(x) end)
    words ++ atoms
  end

  defp valid_color?(color), do: Enum.member?(valid_colors, color)

#  defp to_atom(elem) when is_binary(elem), do: String.to_atom(elem)
#  defp to_atom(elem), do: elem
#
#  defp to_chardata(list) when is_list(list), do: list
#  defp to_chardata(other), do: to_string(other)
end
