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
      |> Enum.sort(fn ({_,c1},{_,c2}) -> c1 > c2 end)
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
    # Output of this stage looks like this:
    # [ "0001.txt", "0002.txt", "0003.txt", ...thousands...]

    |> Stream.map(fn(filename) ->
         fn ->
          text = Path.join(sample_dir, filename)
                |> File.read!
          title = String.split(text, "\n", parts: 2) |> List.first
          words = tokenize(text)
          wordmap = Enum.reduce(words, %{}, fn (word,counts) ->
                                             Map.update(counts, word, 1, &(&1+1))
                                           end)
          {title, wordmap}
         end
       end)
    # list of functions that load a file and return:
    # { "American football", %{ "american" => 5, "football" => 3, ... }}
    #
    # Run the load files in parallel (must use Enum.map, not Stream.map)
    |> Enum.map(&Task.async/1)

    # Wait for async tasks to finish, okay to use stream here
    |> Stream.map(&Task.await/1)

    # Reduce each {title, countmap} into the map of all words
    |> Enum.reduce(%{},
        fn ({title,wordmap},counts) ->
           # Reduce each member of the word map into the map of all words
           Enum.reduce(wordmap, counts,
               fn ({word,count},c) ->
                  Map.update(c, word, [{title,count}], &([{title,count}|&1]))
               end)
        end)
  end
end
