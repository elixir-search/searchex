defmodule Searchex.Adapter.Shake.Filesys do

  use Shake.Module

  step :validate_file_root_presence
  step :validate_file_roots
  step :expand_file_roots

  def validate_file_root_presence(frame, _opts) do
    alias Searchex.Request.Util.Helpers
    case length(frame.params.adapter.file_roots) do
      0 -> halt(frame, "No file_roots")
      _ -> frame
    end
  end

  # halt if one or more of the file roots is missing
  def validate_file_roots(frame, _opts) do
    alias Searchex.Request.Util.Helpers
    badpaths = frame.params.adapter.file_roots
               |> Enum.map(fn(path) -> {Path.expand(path, Helpers.repo_dir(frame)), path} end)
               |> Enum.map(fn({full_path, path}) -> {File.exists?(full_path), path} end)
               |> Enum.filter(fn(tup) -> ! elem(tup, 0) end)
               |> Enum.map(fn(tup) -> elem(tup, 1) end)
               |> Enum.join(", ")
    case badpaths do
      "" -> frame
      _  -> halt(frame, "Missing path (#{badpaths})")
    end
  end

  def expand_file_roots(frame, _opts) do
    alias Searchex.Request.Util.Helpers
    new_roots  = Helpers.expanded_file_roots(frame)
    new_params = put_in(frame.params, [Access.key(:adapter, nil), :file_roots], new_roots)
    %Frame{frame | params: new_params}
  end
end
