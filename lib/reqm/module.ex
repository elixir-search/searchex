defmodule Reqm.Module do

  @moduledoc false

  @type step :: module | atom

  defmacro __using__(opts) do
    quote do
      @behaviour Reqm
      @step_module_opts unquote(opts)

      def init(opts) do
        opts
      end

      def call(frame, opts) do
        step_module_call(frame, opts)
      end

      defoverridable [init: 1, call: 2]

      use Reqm.Validate

      alias  Reqm.Frame
      import Reqm.Frame
      import Reqm.Module, only: [step: 1, step: 2]

      Module.register_attribute(__MODULE__, :steps, accumulate: true)
      @before_compile Reqm.Module
    end
  end

  defmacro __before_compile__(env) do
    steps       = Module.get_attribute(env.module, :steps)
    module_opts = Module.get_attribute(env.module, :step_module_opts)

    {frame, body} = Reqm.Module.compile(env, steps, module_opts)

    quote do
      defp step_module_call(unquote(frame), _), do: unquote(body)
    end
  end

  defmacro step(step, opts \\ []) do
    quote do
      @steps {unquote(step), unquote(opts), true}
    end
  end

  def compile(env, job, module_opts) do
    frame = quote do: frame
    {frame, Enum.reduce(job, frame, &quote_step(init_step(&1), &2, env, module_opts))}
  end

  # Initializes the options of a step at compile time.
  defp init_step({step, opts, guards}) do
    case Atom.to_char_list(step) do
      ~c"Elixir." ++ _ -> init_module_step(step, opts, guards)
      _                -> init_fun_step(step, opts, guards)
    end
  end

  defp init_module_step(step, opts, guards) do
    initialized_opts = step.init(opts)

    if function_exported?(step, :call, 2) do
      {:module, step, initialized_opts, guards}
    else
      raise ArgumentError, message: "#{inspect step} step must implement call/2"
    end
  end

  defp init_fun_step(step, opts, guards) do
    {:function, step, opts, guards}
  end

  # `acc` is a series of nested step calls in the form of
  # step3(step2(step1(frame))). `quote_step` wraps a new step around that series
  # of calls.
  defp quote_step({step_type, step, opts, guards}, acc, env, module_opts) do
    call = quote_step_call(step_type, step, opts)

    error_message = case step_type do
      :module   -> "expected #{inspect step}.call/2 to return a Reqm.Frame"
      :function -> "expected #{step}/2 to return a Reqm.Frame"
    end <> ", all steps must receive a frame and return a frame"

    {fun, meta, [arg, [do: clauses]]} =
      quote do
        case unquote(compile_guards(call, guards)) do
          %Reqm.Frame{halted: true} = frame ->
            unquote(log_halt(step_type, step, env, module_opts))
            frame
          %Reqm.Frame{} = frame ->
            unquote(acc)
          _ ->
            raise unquote(error_message)
        end
      end

    generated? = :erlang.system_info(:otp_release) >= '19'

    clauses =
      Enum.map(clauses, fn {:->, meta, args} ->
        if generated? do
          {:->, [generated: true] ++ meta, args}
        else
          {:->, Keyword.put(meta, :line, -1), args}
        end
      end)

    {fun, meta, [arg, [do: clauses]]}
  end

  defp quote_step_call(:function, step, opts) do
    quote do: unquote(step)(frame, unquote(Macro.escape(opts)))
  end

  defp quote_step_call(:module, step, opts) do
    quote do: unquote(step).call(frame, unquote(Macro.escape(opts)))
  end

  defp compile_guards(call, true) do
    call
  end

  defp compile_guards(call, guards) do
    quote do
      case true do
        true when unquote(guards) -> unquote(call)
        true -> frame
      end
    end
  end

  defp log_halt(step_type, step, env, module_opts) do
    if level = module_opts[:log_on_halt] do
      message = case step_type do
        :module   -> "#{inspect env.module} halted in #{inspect step}.call/2"
        :function -> "#{inspect env.module} halted in #{inspect step}/2"
      end

      quote do
        require Logger
        # Matching, to make Dialyzer happy on code executing Reqm.Builder.compile/3
        _ = Logger.unquote(level)(unquote(message))
      end
    else
      nil
    end
  end
end
