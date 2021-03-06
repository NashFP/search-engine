# Elindex - full text search in Elixir

This is the program we wrote collaboratively at the August 2016 meeting.
@tattdcodemonkey, @joshcrews, @digitalBush, and @jorendorff worked on it,
among others.

To try it out:

Make sure you already have the data in the root:

```sh
pushd ..
wget -O sample.tar.bz2 http://bit.ly/2avfASU
tar xjvf sample.tar.bz2
popd
```

Then you can just run the tests:

```sh
mix test
```

This will load the files, search for 'launch', and confirm that it has the
results it should have.  If you wanted to do this in the shell, you could do the
following:

```sh
iex -S mix
```

<pre>$ <strong>iex -S mix</strong>
Erlang/OTP 19 [erts-8.0] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Compiling 2 files (.ex)
Generated elindex app
Interactive Elixir (1.3.2) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)&gt; <strong>Elindex.Searcher.load_files()</strong>
{:ok, #PID<0.148.0>}
iex(2)&gt; <strong>Elindex.Searcher.search("launch")</strong>
[{"1971", 4}, {"2012–13 NHL season", 2}, {"Antrim GAA", 1},
 {"Big Ten Network", 8}, {"Boris Becker", 1}, {"Bryan Adams", 1},
 {"ESPN.com", 2}, {"Huntsville, Alabama", 1}, {"IGN", 3}, {"Johor Bahru", 1}]
</pre>

**NOTE: We are cheating** by only loading the first 3,000 words of each of the first 300 files.
Loading the whole sample would take too long - the program needs tuning.

