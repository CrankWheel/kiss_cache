defmodule KissCache.MixProject do
  use Mix.Project

  @version "1.0.0"
  @source_url "https://github.com/CrankWheel/kiss_cache"

  def project do
    [
      app: :kiss_cache,
      version: @version,
      language: :erlang,
      description: description(),
      package: package(),
      deps: deps(),
      docs: docs(),
      test_paths: ["test"],
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_options: erlc_options(Mix.env())
    ]
  end

  defp erlc_options(:test), do: [:debug_info, {:d, :TEST}]
  defp erlc_options(_), do: [:debug_info]

  defp elixirc_paths(:test), do: ["test/support"]
  defp elixirc_paths(_), do: []

  def application do
    [
      extra_applications: extra_applications(Mix.env())
    ]
  end

  defp extra_applications(:test), do: [:eunit, :elixir]
  defp extra_applications(_), do: []

  defp deps do
    [
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp description do
    """
    A simple, fast ETS-based cache with timed expiry for Erlang and Elixir.

    Provides both basic put/get operations and serialized fetch operations to minimize
    thundering herd problems when caching expensive computations.
    """
  end

  defp package do
    [
      name: "kiss_cache",
      files: ~w(src mix.exs README.md LICENSE CHANGELOG.md),
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      source_url: @source_url,
      extras: ["README.md"]
    ]
  end
end
