an elixir backend for gleam

```elixir
defmodule :n do
  import Kernel, except: [min: 2, max: 2, length: 1, to_string: 1]

  def send_pkt() do
    :erlang.error(%{:gleam_error => :todo,
        :message => "This has not yet been implemented",
        :module => <<"n"::utf8>>,
        :function => "send_pkt",
        :line => 33})

  end

  def wait_pkt(cont) do
    %{ __module__: :n, __type__: WaitPkt, cont: :'gleam.dynamic'.from(cont) }

  end

  def plan(a) do
    send_pkt()
    wait_pkt((fn(res) -> %{ __module__: :n, __type__: Done, field_0: Success }
         end))

  end

  def main() do
    :'gleam.io'.println("Hello from n!")
    a = %{ __module__: :n, __type__: Rolf, field_0: 1, name1: 2, name2: "so" }

    b = %{ a | name1: 3 }

    :'gleam.io'.println(
      %{ __module__: :n, __type__: Rolf, field_0: 0, name1: 1, name2: "" }.name2
    )
    :'gleam.io'.println(b.name2)
    case b do
      %{ __type__: Rolf, name1: 1, name2: n0 } ->
        1
    end
    %{ __type__: Rolf, name2: p } = b

    case b do
      n when n.name2 == "" ->
        1
    end

  end
end
```

