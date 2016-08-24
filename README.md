# NashFP search engine project

Let's write a search engine!

Start by downloading the sample data (133 MB download, unzips to 492 MB):

    wget -O sample.tar.bz2 http://bit.ly/2avfASU

    tar xjf sample.tar.bz2              # creates "sample" directory

The `sample` directory contains about 8,000 Wikipedia articles.
They're mostly around 50-100 KB of text each. One is 400 KB.


## The goal

Write a program that lets the user search for words in the `sample` files.
The simplest approach would be to use `grep`:

    grep -i elixir sample/*.txt

This works (try it out!), but there are several things to improve on here:

*   **Query speed.** `grep` scans every line of every file. This is already
    slow for our sample, and the real Wikipedia is much larger.
    It's OK for our program to take seconds or even minutes to get
    started, but once that's done, individual queries should be blazing fast.

* - **Search quality.** `grep` doesn't tell us which document mentions
    "traffic" the most (Bangkok) or which ones mention both "traffic"
    and "waffles" (in this sample, only "Brussels" and "Snoop Dogg").
    It doesn't sort results by relevance.

*   **Result presentation.** `grep` doesn't even show the title of the
    articles. We can do better.

All this may sound really hard. But it's not! This repository contains some
sample code to help you get started.


## Write your own

Join the fun! Add a directory to this repo containing your code.

If you don't already have the right permissions to push to this repo,
file an issue! We'll hook you up.

By convention, we use directory names that tell who wrote the code
and what language it's in, separated by a `+`: `jorendorff+elixir`
contains @jorendorff's Elixir code.
