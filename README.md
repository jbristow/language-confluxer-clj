# language-confluxer-clj

This is a clojure adaptation of the Language Confluxer perl scripts developed by [Chris Pound](http://generators.christopherpound.com/).

## Requirements

* Install [leiningen](https://leiningen.org/).

## Usage

The Language Confluxer (or lc for short) takes in a sample wordlist and
generates a map of tuples to the letters that follow them. Using this map, we
can create random words that resemble the sample input.

### The Language Confluxer

```bash
$ lein run -- lc -n 10 -f datafile.txt
```

This reads a wordlist, does some cleanup, and then generates words to standard
out.  The above example prints out 10 words. Output counts above 10000 may take
awhile, and expect to wait a few seconds for inputs over a megabyte.

### Write trigram map to file.

```bash
$ lein run -- compress -f datafile.txt -o datafile.txt.out
```

This does the beginning part of `lc`, but instead of generating words, it dumps
the data to the outfile. I'm still testing whether reading these compressed
files is faster than just reading thw whole input again, but it *seems* likely
that it will be. It's interesting to look at for large datasets though!

## Options

```
  -n, --number NUMBER  1  Number of words to generate.
  -f, --file FILE         Filename of sample wordlist.
  -d, --data FILE         Filename of compressed datafile.
  -o, --output FILE       Filename to write compressed datafile to.
  -h, --help
```

### Bugs

...

## License

Copyright © 2017 Jon Bristow

Distributed under the MIT Public License. Please see LICENSE for details.
