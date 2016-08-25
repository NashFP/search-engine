defmodule FunctionalTest do
  use ExUnit.Case

  test "the searcher works" do
    IO.puts "Let's load the files up mnkay?"
    {:ok, _pid} = Elindex.Searcher.load_files()
    IO.puts "Wonder which documents contain the word 'launch'"
    results = Elindex.Searcher.search("launch")
    IO.puts "Here they are! \\o/ 🎉"
    IO.inspect results
    assert results |> Enum.member?({"2012–13 NHL season", 2})
  end
end
