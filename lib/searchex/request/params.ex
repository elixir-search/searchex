defmodule Searchex.Request.Params do

  @moduledoc false

  alias Searchex.Request.Util.Validations
  use Shreq.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  validation_list = [
    &Validations.cfg_nomatch?/2        ,
    &Validations.cfg_ambiguous?/2      ,
  ]

  step :validate, with: [&Validations.cfg_snip_invalid?/2]
  step :validate, with: validation_list
  step :generate_cfg_name
  step :generate_params
  step :validate_matching_cfg_names
  step Shreq.Proxy, :docsrc
  step :generate_digest

  def generate_cfg_name(frame, _opts) do
    cfg_name = frame.cfg_snip |> Searchex.Config.CfgHelpers.cfg_name
    %Frame{frame | cfg_name: cfg_name}
  end

  def generate_params(frame, _opts) do
    params = frame.cfg_name
             |> Searchex.Config.cat
             |> Searchex.Config.Load.to_map
             |> Util.Ext.Map.atomify_keys
             |> Searchex.Request.Build.Catalog.Params.create_from_cfg
    %Frame{frame | params: params}
  end

  def validate_matching_cfg_names(frame, _opts) do
    frame_name = String.split(frame.cfg_name, "/") |> Enum.at(1)
    coll_name  = frame.params.collection
    case frame_name == coll_name do
      true -> frame
      _    -> halt(frame, "Mismatched collection name ('#{frame_name}' vs '#{coll_name}')")
    end
  end

  def generate_digest(%Frame{cfg_name: cfg_name} = frame, _opts) do
    alias Searchex.Config.CfgHelpers
    alias Util.TimeStamp
    term   = TimeStamp.filepath_timestamp(CfgHelpers.cfg_file(cfg_name))
    digest = Util.Ext.Term.digest({cfg_name, term, get_digest(frame, :docsource)})
    set_digest(frame, :params, digest)
  end
end
