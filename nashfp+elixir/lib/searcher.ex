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

  def tokenize(text) do
    Regex.scan(~r(\b[A-Za-z]+\b), text)
    |> Enum.map(&List.first/1)
    |> Enum.map(&String.downcase/1)
  end

  def load_all_files do
    sample_dir = "../sample"

    File.ls!(sample_dir)
    |> Enum.take(300)  # HACK 1 - limit to 300 files
    # Output of this stage looks like this:
    # [ "0001.txt", "0002.txt", "0003.txt", ...thousands...]

    |> Enum.map(fn(filename) ->
         text = Path.join(sample_dir, filename)
                |> File.read!
         title = String.split(text, "\n", parts: 2) |> List.first
         words = tokenize(text)
                 |> Enum.take(3000)  # HACK 2 - limit to first 3,000 words
         {title, words}
       end)
    # [
    #    { "American football", [ "american", "football", ... ] },
    #    { "Snoop Dogg", [ "brussels", "belgium", ... ] },
    #    ...
    # ]

    |> Enum.flat_map(fn({title, words}) ->
         Enum.map(words, fn (word) -> {title, word} end)
       end)
    # [
    #    { "American football", "american" },
    #    { "American football", "football" },
    #    { "Snoop Dogg", "brussels" },
    #    ... zillions ...
    # ]

    |> Enum.group_by(fn ({_, word}) -> word end,
                     fn ({title, _}) -> title end)
    # %{
    #   "waffles" => ["Belgium", "Snoop Dogg", ...],
    #   "traffic" => ["Belgium", "Myspace", ...],
    #   ... millions more terms ...
    # }

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
    # [
    #   {"waffles", %{"Belgium" => 4, "Snoop Dogg" => 7, ...}},
    #   {"traffic", %{"Belgium" => 7, "Myspace" => 3, ...}},
    #   ... millions ...
    # ]

    # %{
    #   "waffles" => %{ "Snoop Dogg" => 5, ... },
    #   "traffic" => %{ "Snoop Dogg" => 2, ... }
    # }
    |> Enum.into(%{})
  end
end
