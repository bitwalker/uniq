uuid_string = "716c654f-d2b7-436b-9751-2440a9cb079d"
uuid_binary = <<113, 108, 101, 79, 210, 183, 67, 107, 151, 81, 36, 64, 169, 203, 7, 157>>

Benchee.run(
  %{
    "elixir_uuid" => fn
      {"to_string", bin} ->
        UUID.binary_to_string!(bin)

      {"to_binary", s} ->
        UUID.string_to_binary!(s)

      {gen, ns, name} ->
        apply(UUID, gen, [ns, name])

      gen ->
        apply(UUID, gen, [])
    end,
    "uniq" => fn
      {"to_string", bin} ->
        Uniq.UUID.to_string(bin)

      {"to_binary", s} ->
        Uniq.UUID.string_to_binary!(s)

      {gen, ns, name} ->
        apply(Uniq.UUID, gen, [ns, name])

      gen ->
        apply(Uniq.UUID, gen, [])
    end
  },
  inputs: %{
    "to_string" => {"to_string", uuid_binary},
    "to_binary" => {"to_binary", uuid_string},
    "uuid1" => :uuid1,
    "uuid3" => {:uuid3, :dns, "my.example.com"},
    "uuid4" => :uuid4,
    "uuid5" => {:uuid5, :dns, "my.example.com"},
  }
)
