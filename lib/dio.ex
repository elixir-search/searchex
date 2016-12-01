defmodule DIO do

  @moduledoc """
  DIO in an IO module with support for debugging.  
  """

  @doc """
  Has the same interface as `IO.inspect` with some additional options.

  - color - one of red, green, blue, yellow, cyan, magenta, white

  - label - just like the label option in Elixir 1.4 - useful within pipes

  - length: 0 - print the label but not the item

  - display: fn(item) -> "OUTPUT" end - custom display function

  """
  def inspect(item, opts \\ []) do
    inspect :stdio, item, opts
  end

  @doc """
  Inspects `item` according to the given options using the IO `device`.
  """
  def inspect(device, item, opts) when is_list(opts) do
    label    = if (label = opts[:label]), do: [to_chardata(label), ": "], else: []
    alt_opts  = struct(Inspect.Opts, opts)
    new_item  = display(item, opts)
    chardata = Inspect.Algebra.format(Inspect.Algebra.to_doc(new_item, alt_opts), alt_opts.width)
    [label, chardata, reset] = modify([label, chardata], opts)
    unless label == "" && chardata == "", do: IO.puts(device, [label, chardata, reset])
    item 
  end

  @doc """
  Generates output in :dev and :prod modes, but not in :test.

  If you want output in test mode, fall back to `IO.puts`.
  """
  def puts(string) do
    case Mix.env do
      :dev  -> IO.puts string
      :prod -> IO.puts string
      :test -> :OK
      _     -> :OK
    end
  end

  @doc """
  Returns the element type.
  """
  def type(item) do
    cond do 
      is_atom(item)      -> :atom
      is_binary(item)    -> :binary
      is_bitstring(item) -> :bitstring
      is_boolean(item)   -> :boolean
      is_float(item)     -> :float
      is_function(item)  -> :function
      is_integer(item)   -> :integer
      is_list(item)      -> :list
      is_map(item)       -> :map
      is_pid(item)       -> :pid
      is_port(item)      -> :port
      is_reference(item) -> :reference
      is_tuple(item)     -> :tuple
      true               -> :unknown
    end
  end

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
    default = [dev: :show, prod: :show, test: :hide]
    modlist = Enum.reduce options, default, fn({k, v}, acc) ->
      if Enum.member?([:test, :prod, :dev], k), do: Keyword.merge(acc, [{k, to_atom(v)}]), else: acc end
    case Keyword.get(modlist, Mix.env) do
      :show  -> {[label, chardata], options}
      _      -> {[""   , ""      ], options}
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
    acolor = to_atom(color)
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
    atoms = Enum.map(words, fn(x) -> String.to_atom(x) end)
    words ++ atoms
  end

  defp valid_color?(color), do: Enum.member?(valid_colors, color)

  defp to_atom(elem) when is_binary(elem), do: String.to_atom(elem)
  defp to_atom(elem), do: elem

  defp to_chardata(list) when is_list(list), do: list
  defp to_chardata(other), do: to_string(other)
end
