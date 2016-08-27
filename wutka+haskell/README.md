# Searcher - A search engine in Haskell

This is a Haskell implementation of the search, using the same basic data
structures as my Elixir solution, which is a modified version of the one
we did at the August 2016 meeting.

It takes almost 4 minutes to load the dictionary, compared to 1 minute for
the Elixir version. This program was a lesson in the difficulties in reasoning
about Haskell performance. I am using Data.Map.Strict and foldl' which are both
supposed to do strict evaluation. When I first ran the program, before making it
do things in batches, it ran out of memory quickly (on a 16G machine).
I eventually figured out that there was a large amount of lazy data that was
taking up the memory. When I added some code to process the files in batches and
traverse the map and print some statistics, the memory utilization was much more
sane. I eventually changed it to use deepseq to traverse the maps rather than
printing the data. I still don't know what the actual problem is, however (assuming
that Data.Map.Strict and foldl' are actually strict).

To build the program:
stack build

To run it:
stack exec searcher-exe

If you play around with trying to track down the lazy data, you may want to run it with
a heap restriction in place, like:
stack exec -- searcher-exe +RTS -M8G -RTS

Once it loads (almost 4 minutes on my 16-core, 16G machine), it will prompt for a search
term and will print the top 10 results:

To try it out:
<pre>$ <strong>stack exec searcher-exe</strong>
Loaded index with 832515 entries
Search term: launch
[("Vandenberg Air Force Base",254),("Cape Canaveral Air Force Station",169),("Space Shuttle",109),("Kennedy Space Center",97),("Dreamcast",53),("NASA",51),("Wii U",43),("Wii",40),("Sega Saturn",38),("Nintendo 3DS",35)]
Search term: 
</pre>
