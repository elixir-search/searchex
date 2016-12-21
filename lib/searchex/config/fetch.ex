defmodule Searchex.Config.Fetch do
  @moduledoc false

  import Searchex.Config.CfgValidations

  def exec(repo) do
    case check_validations(validation_list(repo)) do
      {:error, msgs} -> {:error, msgs}
      {:ok}          -> clone(repo)
    end
  end

  # -----

  defp validation_list(repo) do
    [
       missing_git?(repo)            ,
       repo_exists?(repo)            ,
       missing_gh_repo?(repo)        ,
    ]
  end
  
  defp clone(repo) do
    File.mkdir_p Searchex.base_dir
    Util.Ext.IO.puts "Searchex base directory: #{Searchex.base_dir}"
    System.cmd("git", ["clone", gh_url(repo)], cd: Searchex.base_dir)
    {:ok}
  end

  # ----- utility methods

  def gh_url(repo) do
    "https://github.com/#{repo}"
  end

  # ----- validations

  def missing_git?(_) do
    case System.cmd("which", ["git"]) do
      {_, 0} -> {:ok}
      _      -> {:error, "GIT not found."}
    end
  end

  def repo_exists?(repo) do
    rdir = String.split(repo, "/") |> List.last
    base = Searchex.base_dir <> "/" <> rdir
    if File.dir?(base), do: {:error, "Repo dir already exists (#{base})"}, else: {:ok}
  end

  def missing_gh_repo?(repo) do
    case System.cmd("git", ["ls-remote", gh_url(repo), ">", "/dev/null"], stderr_to_stdout: true) do
      {_, 0} -> {:ok}
      _      -> {:error, "GitHub repo not found (#{gh_url(repo)})"}
    end
  end
end