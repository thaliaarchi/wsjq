# wsjq — Whitespace interpreter in jq

wsjq is an interpreter and debugger for the Whitespace programming language,
written in jq.

## Usage

```sh
wsjq --help
wsjq hworld.ws
wsjq run hworld.ws
whoami > name.txt
wsjq debug --in-file=name.txt --no-prompt name.ws
wsjq disasm fact.ws
wsjq --jq=gojq hworld.ws
```

wsjq has been tested with jq version 1.6 and gojq commit
[77b3bcd](https://github.com/itchyny/gojq/commit/77b3bcd8d460718540fc57f1e45f86ac7c6bb9ef)
([gojq#186](https://github.com/itchyny/gojq/issues/186)).

## Real-time I/O

wsjq prints to stdout by generating a stream of strings that are immediately
emitted, rather than by appending to a buffer. wsjq sets the main input source
to be stdin so that user input is streamed line-by-line with `input`. The
contents of the Whitespace source file are then bound to the `$src` variable
with `--rawfile`.

## Limitations

- Integers have 53 bits of precision, because jq uses IEEE 754 64-bit floating
  point.
- readc collapses CRLF to LF on Windows, because `input` strips line endings
  according to the current OS.

## See also

[bf.jq](https://github.com/itchyny/brainfuck/blob/master/bf.jq) by itchyny
([blog post](https://itchyny.medium.com/json-formatter-written-in-jq-b716c281afd7))
and [bf.jq](https://github.com/MakeNowJust/bf.jq/blob/master/bf.jq) by
MakeNowJust are Brainfuck interpreters written in jq, though neither support the
`,` read operator and both display `.` output on program termination.

The jq standard library filters defined in
[builtin.jq](https://github.com/stedolan/jq/blob/master/src/builtin.jq) are a
helpful reference for learning advanced usage.

## License

This project is made available under the
[Mozilla Public License, v. 2.0](https://mozilla.org/MPL/2.0/).
