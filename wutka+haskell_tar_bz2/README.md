# Wikisearch - A search engine in Haskell

When we started the search engine exercise in July, I had the idea
to try reading the .tar.bz2 file in-place rather than unpacking it.
The idea was that because it was compressed, reading it in-place might
actually be as fast as spawning threads to read separate files since
there would be less disk I/O.

I got bogged down by the Haskell Tar library because it was returning
a lazy byte string of Word8 and I needed to work with characters.
I finally went back to it and figured out how to get the Tar entry
into a Text value, which allowed me to finish, using a technique similar
to what I did for my earlier Haskell version. It seems to use a little
more memory than the earlier one, but it runs at roughly the same speed,
loading in a little under 3 minutes. It also seems to use multiple CPUs
even though I don't do any explicit parallelization.

To build the program:
stack build

You should make sure that the parent directory has sample.tar.bz2
If not, you can package the sample directory with:
tar cjSf sample.tar.bz2 sample

Then, run it:
stack exec wikisearch-exe

It really needs close to 8G of space, it dies when it only has 6. You
don't need to explicitly limit it like this, but it's a good idea
stack exec -- wikisearch-exe +RTS -M8G

Once it loads, it will prompt for a search
term and will print the top 10 results:

To try it out:
<pre>$ <strong>stack exec wikisearch-exe</strong>
Loaded wikipedia with 870599 entries
Search term: jacket
[("Masters Tournament",23),("Stanley Kubrick",15),("United States Army Air Forces",15),("Phil Collins",13),("Fencing",13),("Georgia Tech Yellow Jackets football",12),("9\215\&19mm Parabellum",10),("Michael Jackson",8),("Royal Canadian Mounted Police",7),("Marlon Brando",5)]
Search term: launch
[("Vandenberg Air Force Base",254),("Cape Canaveral Air Force Station",169),("Space Shuttle",109),("Kennedy Space Center",97),("Dreamcast",53),("NASA",51),("Wii U",43),("Wii",40),("Sega Saturn",38),("Nintendo 3DS",35)]
Search term: 
</pre>
