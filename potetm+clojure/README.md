Just java:
```
java -cp "${CLOJURE_JAR}":src clojure.main -m com.potetm.search-engine /path/to/wikipedia/sample
```

With lein:
```
lein run -m com.potetm.search-engine /path/to/wikipedia/sample
```

Example usage (press `ctrl-c` to exit):
```
/Users/potetm/projects/nashfp/search-engine/potetm+clojure> java -cp "${CLOJURE_JAR}":src clojure.main -m com.potetm.search-engine /Users/potetm/tmp/sample/
building index...
finished building index, took: 49561 msecs
> sword
    /Users/potetm/tmp/sample/0514.txt - Fencing - 9
    /Users/potetm/tmp/sample/7814.txt - KGB - 9
    /Users/potetm/tmp/sample/7053.txt - Operation Overlord - 9
    /Users/potetm/tmp/sample/0817.txt - King Arthur - 8
    /Users/potetm/tmp/sample/4404.txt - Royal Engineers - 8
    /Users/potetm/tmp/sample/5970.txt - Jesus - 7
    /Users/potetm/tmp/sample/0411.txt - Arthur Wellesley, 1st Duke of Wellington - 7
    /Users/potetm/tmp/sample/7437.txt - United States Marine Corps - 7
    /Users/potetm/tmp/sample/0339.txt - Celts - 7
    /Users/potetm/tmp/sample/4734.txt - Role-playing video game - 7
>
```

Note that I cheated a little bit by using parallelization :)

To see an apples-to-apples comparison with the python version, change the `pmap`
on line 29 to a `map`.
