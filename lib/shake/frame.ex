defmodule Shake.Frame do
  @moduledoc """
  Defines a struct that is passed up and down the Shake build chain.

        defstruct cfg_name: "",
                  params:   %{},
                  catalog:  %{},
                  index:    %{},
                  query:    "",
                  scores:   [],
                  results:  %{},
                  halted:   false,
                  halt_msg: "",
                  digests:  %{},
                  assigns:  %{}
  """

  alias Shake.Frame

  @type cfg_name :: String.t
  @type params   :: map
  @type catalog  :: map
  @type index    :: map
  @type query    :: String.t
  @type scores   :: list
  @type results  :: map
  @type halted   :: boolean
  @type halt_msg :: String.t | [String.t]
  @type digests  :: %{atom => String.t}
  @type assigns  :: %{atom => any}

  @type t :: %__MODULE__{
             cfg_name:  cfg_name,
             params:    params,
             catalog:   catalog,
             index:     index,
             query:     query,
             scores:    scores,
             results:   results,
             halted:    halted,
             halt_msg:  halt_msg,
             digests:   digests,
             assigns:   assigns
           }
             
  defstruct cfg_name: "",
            params:   %{},
            catalog:  %{},
            index:    %{},
            query:    "",
            scores:   [],
            results:  %{},
            halted:   false,
            halt_msg: "",
            digests:  %{},
            assigns:  %{}

  @doc """
  Halts the Shake job by preventing downstream steps 
  from being invoked.  Optional `halt_msg` can be a 
  String or list of Strings.
  """
  @spec halt(t) :: t
  def halt(%Frame{} = frame, halt_msg \\ "") do
    %{frame | halted: true, halt_msg: halt_msg}
  end

  @doc """
  Assigns a value to a key in the frame

  ## Examples

      iex> frame.assigns[:hello]
      nil
      iex> frame = assign(frame, :hello, :world)
      iex> frame.assigns[:hello]
      :world

  """
  @spec assign(t, atom, term) :: t
  def assign(%Frame{assigns: assigns} = frame, key, value) when is_atom(key) do
    %Frame{frame | assigns: Map.put(assigns, key, value)}
  end

  @doc """
  Assign a digest to a key in the frame
  """
  def set_digest(%Frame{digests: digests} = frame, key, value) when is_atom(key) do
    %Frame{frame | digests: Map.put(digests, key, value)}
  end

  @doc """
  Retrieve the digest from the frame
  """
  def get_digest(%Frame{digests: digests} = _frame, key) when is_atom(key) do
    digests[key]
  end
end

