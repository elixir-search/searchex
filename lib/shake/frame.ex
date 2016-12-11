defmodule Shake.Frame do
  @moduledoc "The Shake Frame"

  alias Shake.Frame

  @type cfg_name :: String.t
  @type params   :: map
  @type catalog  :: map
  @type index    :: map
  @type results  :: map
  @type halted   :: boolean
  @type halt_msg :: String.t | [String.t]

  @type t :: %__MODULE__{
             cfg_name: cfg_name,
             params:   params,
             catalog:  catalog,
             index:    index,
             results:  results,
             halted:   halted
           }
             
  defstruct cfg_name: "",
            params:   %{},
            catalog:  %{},
            index:    %{},
            results:  %{},
            halted:   false,
            halt_msg: ""

  @doc """
  Halts the Shake job by preventing downstream steps 
  from being invoked.  Optional `halt_msg` can be a 
  String or list of Strings.
  """
  @spec halt(t) :: t
  def halt(%Frame{} = frame, halt_msg \\ "") do
    %{frame | halted: true, halt_msg: halt_msg}
  end
end

