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

Conversion from `ByteString` to DOM

| Name | Language | Time |
|------|----------|------|
| [hexml][] | C | 1.787 ms |
| [xeno][] | Haskell| 4.307 ms |
| [libxml][] | C | 21.69 ms |
| [hexpat][] | C | 133.4 ms |
| [xml-conduit][] | Haskell | 173.7 ms |

Conversion from `ByteString` to a list of SAX events

| Name | Language | Time |
|------|----------|------|
| [xeno][] | Haskell | 25.15 ms |
| [sax][] | Haskell | 34.69 ms |
| [hexpat][] | C | 74.84 ms |
| [libxml-sax][] | C | 110.2 ms |
| [xml-conduit][] | Haskell | 196.1 ms |

Conversion from DOM to `Data.Book.Root` data type

| Name | Language | Time |
|------|----------|------|
| [sax][] | Haskell | 56.13 ms |
| [xmlbf-xeno][] | Haskell | 137.8 ms |
| [dom-parser][] | Haskell | 132.9 ms |

Conversion from `ByteString` to `Data.Book.Root` data type

| Name | Language | Time |
|------|----------|------|
| [sax][] | Haskell | 66.55 ms |
| [xmlbf-xeno][] | Haskell | 147.6 ms |
| [dom-parser][] | Haskell | 267.5 ms |

## Space

``` bash
stack test
```

Conversion from `ByteString` to DOM

| Case           | Allocated   | GCs |
|----------------|-------------|-----|
| hexml          |   1,040,416 |   0 |
| xeno           |   8,184,976 |   2 |
| hexpat         |  95,849,656 |  91 |
| xml-conduit    | 535,804,344 | 519 |

Conversion from DOM to `Data.Book.Root` data type

| Case           | Allocated   | GCs |
|----------------|-------------|-----|
| dom/sax        | 201,020,688 | 194 |
| dom/xmlbf-xeno | 399,888,064 | 387 |
| dom/dom-parser | 329,051,944 | 317 |

Conversion from `ByteString` to `Data.Book.Root` data type

| Case           | Allocated   | GCs |
|----------------|-------------|-----|
| bs/sax         | 201,022,120 | 194 |
| bs/xmlbf-xeno  | 408,072,976 | 390 |
| bs/dom-parser  | 864,856,312 | 836 |


[dom-parser]: https://hackage.haskell.org/package/dom-parser
[hexml]: https://hackage.haskell.org/package/hexml
[hexpat]: https://hackage.haskell.org/package/hexpat
[libxml]: https://hackage.haskell.org/package/libxml
[libxml-sax]: https://hackage.haskell.org/package/libxml-sax
[sax]: https://hackage.haskell.org/package/sax
[xeno]: https://hackage.haskell.org/package/xeno
[xmlbf-xeno]: https://hackage.haskell.org/package/xmlbf-xeno
[xml-conduit]: https://hackage.haskell.org/package/xml-conduit
