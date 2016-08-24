#!/usr/bin/env python3
# search.py - An in-memory search engine.

import glob, re, collections, time

def measure_call(fn, *args):
    start = time.time()
    try:
        return fn(*args)
    finally:
        print("({:.6} seconds)".format(time.time() - start))

# Load the files into an in-memory index.
class Index:
    def __init__(self):
        print("Loading data (this takes a minute or two)...")
        self.filenames = glob.glob("../sample/*.txt")
        self.titles = []
        self.index = collections.defaultdict(lambda: collections.defaultdict(int))
        for doc_id, filename in enumerate(self.filenames):
            with open(filename) as f:
                text = f.read()
            self.titles.append(text.split("\n", 1)[0])
            for word in re.findall(r'[0-9a-z]+', text.lower()):
                self.index[word][doc_id] += 1
        print("...done")

    def handle_query(self, query):
        if query not in self.index:
            print("no hits")
        else:
            hits = sorted(self.index[query].items(),
                          key=lambda pair: pair[1],
                          reverse=True)
            for doc_id, hit_count in hits[:10]:
                print("    {} - {} - {}".format(self.filenames[doc_id], self.titles[doc_id], hit_count))
            if len(hits) > 1:
                print("{} hits.".format(len(hits)))

def main():
    index = measure_call(Index)
    while True:
        try:
            query = input("> ").strip()
        except EOFError as exc:
            break
        if query:
            measure_call(index.handle_query, query)

if __name__ == '__main__':
    main()


