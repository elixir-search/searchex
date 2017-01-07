defmodule Searchex.Command.ParamsNew do

  @moduledoc false

  alias Searchex.Command.CmdValidations
  alias Searchex.Command.Build.Catalog.Params
  use Shake.Module

  @doc """
  The API for the module - takes a config name and returns
  a frame with the Params slot filled.
  """
  def exec(cfg_snip) do
    call(%Frame{cfg_snip: cfg_snip}, [])
  end

  validation_list = [
    &CmdValidations.cfg_nomatch?/2        ,
    &CmdValidations.cfg_ambiguous?/2      ,
  ]

  step :validate, with: [&CmdValidations.cfg_snip_invalid?/2]
  step :validate, with: validation_list
  step :generate_cfg_name
  step :generate_params
  step :validate_file_root_presence
  step :validate_file_roots
  step :validate_matching_cfg_names
  step :expand_file_roots
  step :generate_digest

  def generate_cfg_name(frame, _opts) do
    cfg_name = frame.cfg_snip |> Searchex.Config.CfgHelpers.cfg_name
    %Frame{frame | cfg_name: cfg_name}
  end

  # read the config file and generate params
  def generate_params(frame, _opts) do
    params = frame.cfg_name
             |> Searchex.Config.cat
             |> Searchex.Config.Load.to_map
             |> Util.Ext.Map.atomify_keys
             |> Searchex.Command.Build.Catalog.Params.create_from_cfg
    %Frame{frame | params: params}
  end

  def validate_file_root_presence(frame, _opts) do
    alias Searchex.Command.CmdHelpers
    case length(frame.params.file_roots) do
      0 -> halt(frame, "No file_roots")
      _ -> frame
    end
  end

  # halt if one or more of the file roots is missing
  def validate_file_roots(frame, _opts) do
    alias Searchex.Command.CmdHelpers
    badpaths = frame.params.file_roots
               |> Enum.map(fn(path) -> {Path.expand(path, CmdHelpers.repo_dir(frame)), path} end)
               |> Enum.map(fn({full_path, path}) -> {File.exists?(full_path), path} end)
               |> Enum.filter(fn(tup) -> ! elem(tup, 0) end)
               |> Enum.map(fn(tup) -> elem(tup, 1) end)
               |> Enum.join(", ")
    case badpaths do
      "" -> frame
      _  -> halt(frame, "Missing path (#{badpaths})")
    end
  end

  def validate_matching_cfg_names(frame, _opts) do
    alias Searchex.Command.CmdHelpers
    frame_name = String.split(frame.cfg_name, "/") |> Enum.at(1)
    coll_name  = frame.params.collection
    case frame_name == coll_name do
      true -> frame
      _    -> halt(frame, "Mismatched collection name ('#{frame_name}' vs '#{coll_name}')")
    end
  end

  def expand_file_roots(frame, _opts) do
    alias Searchex.Command.CmdHelpers
    new_roots  = CmdHelpers.expanded_file_roots(frame)
    new_params = %Params{frame.params | file_roots: new_roots}
    %Frame{frame | params: new_params}
  end

  # generate a digest for the params
  # the digest is simply the newest timestamp of the config file
  # and all the documents in the file_roots
  def generate_digest(%Frame{cfg_name: cfg_name} = frame, _opts) do
    alias Searchex.Config.CfgHelpers
    alias Searchex.Command.CmdHelpers
    alias Util.TimeStamp
    term  = [CfgHelpers.cfg_file(cfg_name)] ++ CmdHelpers.file_list(frame)
            |> Enum.map(fn(file) -> TimeStamp.filepath_timestamp(file) end)
            |> TimeStamp.newest
    digest = Util.Ext.Term.digest({cfg_name, term})
    set_digest(frame, :params, digest)
  end
end
