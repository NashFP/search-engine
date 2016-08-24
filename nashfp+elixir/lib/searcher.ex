defmodule Elindex.Searcher do
  def load_files do
    Agent.start_link(&load_all_files/0, name: :wikipedia)
  end

  def search(term) do
    Agent.get(:wikipedia, fn (data) ->
      # %{
      #   "waffles" => %{ "Snoop Dogg" => 5, ... },
      #   "traffic" => %{ "Snoop Dogg" => 2, ... }
      # }
      Map.get(data, term, [])
      |> Enum.take(10)
    end)
  end

  def load_all_files do
    # [ 
    #    { "American football", [ "american", "football", ... ] },
    #    { "Snoop Dogg", [ "brussels", "belgium", ... ] }
    # ]
    File.ls!("sample")
    |> Enum.take(300)
    |> IO.inspect
    |> Enum.map(fn(filename) ->
      text = Path.join("sample", filename)
             |> File.read!
      title = String.split(text, "\n", parts: 2) |> List.first
      words = tokenize(text) |> Enum.take(3000)
      {title, words}
    end)

    |> IO.inspect

    # [
    #    { "American football", "american" },
    #    { "American football", "football" },
    #    { "Snoop Dogg", "brussels" },
    #    ... millions ...
    # ]
    |> Enum.flat_map(fn({title, words}) ->
         Enum.map(words, fn (word) -> {title, word} end)
       end)

    |> IO.inspect

    # %{
    #   "waffles" => ["Belgium", "Snoop Dogg", ...],
    #   "traffic" => ["Belgium", "Myspace", ...]]
    #  }
    |> Enum.group_by(fn ({_, word}) -> word end,
                     fn ({title, _}) -> title end)
    |> IO.inspect

    |> Enum.map(fn ({term, titles}) ->
         {
           term,
           Enum.reduce(titles,
                       %{},
                       fn (title, counts) ->
                         Map.update(counts, title, 1, &(&1 + 1))
                       end)
         }
       end)
    |> IO.inspect

    # %{
    #   "waffles" => %{ "Snoop Dogg" => 5, ... },
    #   "traffic" => %{ "Snoop Dogg" => 2, ... }
    # }
    |> Enum.into(%{})
  end

  def tokenize(text) do
    Regex.scan(~r(\b[A-Za-z]+\b), text)
    |> Enum.map(&List.first/1)
    |> Enum.map(&String.downcase/1)
  end

  ## def search(word) do
  ##        |> hit_info(word)
  ##   |> Stream.filter(fn({count, _}) -> count > 0 end)
  ##   |> Enum.sort(&>=/2)
  ##   |> Enum.take(10)
  ## end

  ## def hit_info(blob, word) do
  ##   count = Enum.count()
  ##   title = String.split(blob, "\n") |> List.first
  ##   {count, title}
  ## end
end