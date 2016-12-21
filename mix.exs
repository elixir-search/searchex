defmodule Searchex.Mixfile do
  use Mix.Project

  @version "0.0.1-alpha.4"

  def project do
    [
      app:     :searchex,
      version: @version,
      elixir:  "~> 1.3",
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
      applications: [:mix, :yaml_elixir, :logger, :table_rex, :eex, :lru_cache]
    ]
  end

  defp deps do
    [
      {:yaml_elixir, "~> 1.1"                },  # yaml parser
      {:table_rex  , "~> 0.8"                },  # table formatter
      {:stem_ex    , "~> 0.0.1"              },  # word-stem generator
      {:lru_cache  , "~> 0.1.0"              },  # ETS-based LRU cache
      {:git_cli    , "~> 0.2"                },  # Git CLI
      {:dialyxir   , "~> 0.4.0", only: :dev  },  #
      {:ex_guard   , "~> 1.1.1", only: :dev  },  # test runner
      {:ex_doc     , "~> 0.14" , only: :dev  },  # doc generator
      {:credo      , "~> 0.4"  , only: :dev  }   # code-style checker
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
