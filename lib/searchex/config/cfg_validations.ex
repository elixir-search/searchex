defmodule Searchex.Config.CfgValidations do

  @moduledoc false

  import Searchex.Config.CfgHelpers

  @doc """
  Checks a set of validation functions

  If all validation functions pass, return `{:ok}`.

  If one or more validation functions fail, return
  `{:error, [list of error messages]}`

  NOTE: validation functions returns one of:
  {:ok}
  {:error, msg}
  """
  def check_validations(validations) when is_list(validations) do
    Enum.reduce validations, {:ok}, fn(x, acc) -> reduce_val(x, acc) end
  end

  def check_validations(validations) when is_function(validations) do
    check_validations(validations.())
  end

  def check_validations(validation), do: check_validations([validation])

  def reduce_val(func, acc) when is_function(func), do: reduce_val(func.(), acc)
  def reduce_val({:error, msg}, {:ok}        ), do: {:error, [msg]       }
  def reduce_val({:error, msg}, {:error, lst}), do: {:error, lst ++ [msg]}
  def reduce_val({:ok}        , {:error, lst}), do: {:error, lst         }
  def reduce_val({:ok   , _xx}, {:error, lst}), do: {:error, lst         }
  def reduce_val({:ok}        , {:ok}        ), do: {:ok                 }
  def reduce_val({:ok}        , {:ok   , lst}), do: {:ok   , lst         }
  def reduce_val({:ok   , val}, {:ok}        ), do: {:ok   , [val]       }
  def reduce_val({:ok   , val}, {:ok   , lst}), do: {:ok   , lst ++ [val]}

  # -----

  # TODO: add a test to check validity of config file...

  def cfg_nomatch?(cfg_snip) do
    err = {:error, "No config file match (#{cfg_snip})"}
    if cfg_name(cfg_snip), do: {:ok}, else: err
  end

  def cfg_ambiguous?(cfg_snip) do
    list = cfg_filter(cfg_snip)
    msg  = "Multi-match (#{cfg_snip} => [#{Enum.join(list, ", ")}])"
    err  = {:error, msg}
    if length(list) > 1, do: err, else: {:ok}
  end

  def cfg_snip_invalid?(cfg_snip) do
    test = Regex.match?(~r/[^0-9A-Za-z-_\/]/, cfg_snip)
    err  = {:error, "Invalid config name (#{cfg_snip})"}
    if test, do: err, else: {:ok}
  end

  def cfg_present?(cfg_snip) do
    err = {:error, "Config already exists (#{cfg_snip})"}
    if File.exists?(cfg_file(cfg_snip)), do: err, else: {:ok}
  end
end
