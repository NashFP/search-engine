# Searcher - A search engine in Haskell

This is a Haskell implementation of the search, using the same basic data
structures as my Elixir solution, which is a modified version of the one
we did at the August 2016 meeting.

I managed to shave a minute off the load time, so now this loads in a
little under 3 minutes on my 8-core machine. Although I use seq in several
places to try to keep things strict, I was able to remove the use of deepseq.
Although the performance is roughly the same, I kept a change where instead of
keeping the entries in a list, and using list concatenation when merging maps
together, I created a tree structure where two trees can be merged just by creating
a new node. The expense of joining them together is deferred until the search time.

Because I was able to reduce the memory consumption, I was able to remove the
batch processing of files and just use mapConcurrently.

The thing that improved the performance was switching the readFile function
I was using from a strict one that returned a String to a strict one that
returns a ByteString (which is much more memory efficient).

To build the program:
stack build

To run it:
stack exec searcher-exe

If you play around with trying to track down the lazy data, you may want to run it with
a heap restriction in place, like:
stack exec -- searcher-exe +RTS -M8G

Once it loads (almost 3 minutes on my 8-core, 16G machine), it will prompt for a search
term and will print the top 10 results:

To try it out:
<pre>$ <strong>stack exec searcher-exe</strong>
Loaded index with 832515 entries
Search term: launch
[("Vandenberg Air Force Base",254),("Cape Canaveral Air Force Station",169),("Space Shuttle",109),("Kennedy Space Center",97),("Dreamcast",53),("NASA",51),("Wii U",43),("Wii",40),("Sega Saturn",38),("Nintendo 3DS",35)]
Search term: 
</pre>
