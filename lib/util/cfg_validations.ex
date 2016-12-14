defmodule Util.CfgValidations do

    @moduledoc false

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

end
