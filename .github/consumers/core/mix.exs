defmodule UniqCoreConsumer.MixProject do
  use Mix.Project

  def project do
    [
      app: :uniq_core_consumer,
      version: "0.1.0",
      deps: [{:uniq, path: System.fetch_env!("UNIQ_PACKAGE")}]
    ]
  end
end
