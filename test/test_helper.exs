Application.ensure_all_started(:ecto)

Code.require_file("support/test_repo.exs", __DIR__)
ExUnit.start()
