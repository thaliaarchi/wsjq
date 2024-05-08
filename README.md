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

wsjq requires at least jq 1.6 or gojq 0.12.9 (for [gojq#186](https://github.com/itchyny/gojq/issues/186)).

To run it with [jqjq](https://github.com/wader/jqjq), checkout the [jqjq-compat](https://github.com/thaliaarchi/wsjq/tree/jqjq-compat)
branch and use `wsjqjq` instead of `wsjq`.

## Real-time I/O

wsjq prints to stdout by generating a stream of strings that are immediately
emitted, rather than by appending to a buffer. wsjq sets the main input source
to be stdin so that user input is streamed line-by-line with `input`. The
contents of the Whitespace source file are then bound to the `$src` variable
with `--rawfile`.

## Limitations from jq

When running with jq, integers have 53 bits of precision, because jq uses 64-bit
floating point numbers, and `readc` collapses CRLF to LF on Windows, because
`input` strips line endings according to the current OS.

Run with gojq (`--jq=gojq`) to use arbitrary-precision integers and preserve
line endings.

## See also

[bf.jq](https://github.com/itchyny/brainfuck/blob/main/bf.jq) by itchyny
([blog post](https://itchyny.medium.com/json-formatter-written-in-jq-b716c281afd7))
and [bf.jq](https://github.com/MakeNowJust/bf.jq/blob/master/bf.jq) by
MakeNowJust are Brainfuck interpreters written in jq, though neither support the
`,` read operator and both display `.` output on program termination.

[jqjq](https://github.com/wader/jqjq) by Mattias Wadman is a jq interpreter in
jq. I have contributed features to help wsjq run in jqjq.

The jq standard library filters defined in
[builtin.jq](https://github.com/jqlang/jq/blob/master/src/builtin.jq) are a
helpful reference for learning advanced usage.

## License

This project is made available under the
[Mozilla Public License, v. 2.0](https://mozilla.org/MPL/2.0/).
