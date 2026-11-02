defmodule UniqEctoConsumer.MixProject do
  use Mix.Project

  def project do
    [
      app: :uniq_ecto_consumer,
      version: "0.1.0",
      deps: [
        {:uniq, path: System.fetch_env!("UNIQ_PACKAGE")},
        {:ecto, System.fetch_env!("ECTO_REQUIREMENT")}
      ]
    ]
  end
end
