defmodule Searchex.Mixfile do
  use Mix.Project

  @version "0.0.4"

  def project do
    [
      app:     :searchex,
      version: @version,
      elixir:  "~> 1.4.0",
      escript: [main_module: Searchex.Cli],
      build_embedded:  Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package:     package(),
      deps:        deps(),

      name: "Searchex",
      source_url:   "https://github.com/elixir-search/searchex",
      homepage_url: "https://github.com/elixir-search/searchex",
      docs: [
        source_ref: @version,
        cannonical: "https://hexdocs/searchex",
        extras: ["README.md", "CHANGELOG.md"]
      ]
    ]
  end

  def application do
    [
      applications: [:mix, :yaml_elixir, :logger, :table_rex, :eex, :lru_cache],
      mod: {Searchex.App, []}
    ]
  end

  defp deps do
    [
      {:yaml_elixir, github: "andyl/yaml-elixir" },  # yaml parser
      {:table_rex  , "~> 0.10"  },              # table formatter
      {:stem_ex    , "~> 0.0.2" },              # word-stem generator
      {:lru_cache  , "~> 0.1.0" },              # ETS-based LRU cache
      {:exactor    , "~> 2.2.3" },              # OTP wrappers
      {:ex_doc     , "~> 0.14"  , only: :dev }, # doc generator
      {:credo      , "~> 0.6"   , only: :dev }, # style checker
      {:ex_guard   , "~> 1.2.0" , only: :dev }, # test runner
    ]
  end

  defp description do
    "A full-text search engine written in pure Elixir."
  end

  defp package do
    [
      name: :searchex,
      maintainers: ["AndyL"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/elixir-search/searchex"}
    ]
  end
end
