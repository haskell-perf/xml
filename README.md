# haskell-perf-xml

Benchmarks for XML parsing libraries.

## Generate input data

`generate` script creates ~1Mb XML file:

``` bash
stack exec generate
```

## Time

Conversion from `ByteString` to DOM

``` bash
stack bench
```

| Name | Time |
|------|------|
| hexml | 1.787 ms |
| xeno | 4.307 ms |
| libxml | 21.69 ms |
| xml-conduit | 173.7 ms |

## Space

``` bash
stack test
```

| Case        |   Allocated |        Max |       Live | GCs |
|-------------|-------------|------------|------------|-----|
| libxml      |         480 |        160 |      4,656 |   0 |
| hexml       |   1,118,744 |        216 |      4,520 |   0 |
| xeno        |   8,184,976 |         64 |      4,344 |   2 |
| xml-conduit | 694,404,776 | 24,452,264 | 72,434,280 | 672 |
