# haskell-perf-xml

Benchmarks for XML parsing libraries.

## Generate input data

`generate` script creates ~1Mb XML file:

``` bash
stack exec generate
```

## Time

``` bash
stack bench
```

Columns:

- Name - name of the package on Hackage
- Language - pure Haskell or a C binding

Conversion from `ByteString` to DOM

| Name | Language | Time |
|------|----------|------|
| hexml | C | 1.787 ms |
| xeno | Haskell| 4.307 ms |
| libxml | C | 21.69 ms |
| hexpat | C | 133.4 ms |
| xml-conduit | Haskell | 173.7 ms |

Conversion from `ByteString` to a list of SAX events

| Name | Language | Time |
|------|----------|------|
| xeno | Haskell | 25.15 ms |
| sax | Haskell | 34.69 ms |
| hexpat | C | 74.84 ms |
| libxml-sax | C | 110.2 ms |
| conduit | Haskell | 196.1 ms |

## Space

``` bash
stack test
```

| Case        |   Allocated |        Max |       Live | GCs |
|-------------|-------------|------------|------------|-----|
| libxml      |         480 |        160 |      4,656 |   0 |
| hexml       |   1,118,744 |        216 |      4,520 |   0 |
| xeno        |   8,184,976 |         64 |      4,344 |   2 |
| hexpat      | 122,216,128 | 16,798,072 | 52,907,688 | 117 |
| xml-conduit | 694,404,776 | 24,452,264 | 72,434,280 | 672 |
