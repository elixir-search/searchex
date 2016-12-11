defmodule Shake.Validate do

  defmacro __using__(_opts) do
    quote do

      import Shake.Frame

      def validate(frame, opts) do
        case Keyword.split(opts, [:with]) do
          {[]          , _opts} -> halt(frame, "Validate requires a 'with: &func/2' option")
          {[with: func], opts2} -> validate(frame, func, opts2)
        end
      end

      def validate(frame, func, opts) when is_atom(func) do
        validate(frame, [func], opts)
      end

      def validate(frame, funclist, opts) when is_list(funclist) do
        Enum.reduce funclist, frame, fn(func, acc) ->
          halt_msg = acc.halt_msg
          newfunc  = cond do
            is_atom(func) -> fn(acc, opts) -> apply(__MODULE__, func, [acc, opts]) end
            true          -> func
          end
          newframe = newfunc.(acc, opts)
          if newframe.halt_msg != halt_msg do
            case halt_msg do
              "" -> %Shake.Frame{newframe | halt_msg: [newframe.halt_msg]}
              _  -> %Shake.Frame{newframe | halt_msg: halt_msg ++ [newframe.halt_msg]}
            end
          else
            newframe
          end
        end
      end

    end

  end

end
