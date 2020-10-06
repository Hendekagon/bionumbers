# bionumbers

Some code for converting the Bionumbers database, which comes for some reason in the form of an HTML table, into EDN

```
(require '[bionumbers.parse :as bp])

(take 3 (bp/html-clj (slurp "data/BioNumbers.xls")))

```

will attempt to parse values and ranges, but there's so
many inconsistent patterns remaining to cover

#### References

[Bionumbers](https://bionumbers.hms.harvard.edu/search.aspx)


[BNID 100986, Milo et al 2010](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2808940/)





## License

Copyright © 2020 Matthew Chadwick

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
