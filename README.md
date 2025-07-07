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

wsjq works with jq 1.8.0, [gojq](https://github.com/itchyny/gojq), and [jaq](https://github.com/01mf02/jaq).

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
jq.

The jq standard library filters defined in
[builtin.jq](https://github.com/jqlang/jq/blob/master/src/builtin.jq) are a
helpful reference for learning advanced usage.

## Upstream improvements

As a large and complex jq program, wsjq has directly driven several improvements
in various jq engines:

- gojq
  - [Bug with nested path and variable-style parameters #186](https://github.com/itchyny/gojq/issues/186):
    fixed by itchyny
- jqjq
  - [Running wsjq #10](https://github.com/wader/jqjq/pull/10):
    add `index/1`, `test/1`, `split/2`, `halt_error/1`, `bsearch/1`, and `in/1`,
    and resolve symlinks
  - [Resolve user-defined filters before intrinsics #13](https://github.com/wader/jqjq/pull/13):
    fix `def debug`
  - [Add string interpolation support](https://github.com/wader/jqjq/commit/4ef71ee36c3b00338d967d323c96a9315dedf0d1)
    by wader
  - [Format json with `JQ_COLORS` #14](https://github.com/wader/jqjq/pull/14):
    add `-M`/`--monochrome-output` and `-j`/`--join-output`
  - [Staged CLI #19](https://github.com/wader/jqjq/pull/19):
    add `-f`/`--from-file`, `--arg`, `--rawfile`, and `--unbuffered`
  - Remaining:
    `-R`/`--raw-input`, `-L`/`--library-path`, and `include`
- jaq
  - [Missing builtins (`bsearch` and more) #293](https://github.com/01mf02/jaq/issues/293):
    add `bsearch` by 01mf02
  - [Incorrect lexing of compound operators with unary minus #294](https://github.com/01mf02/jaq/issues/294):
    fixed by 01mf02
  - [Convert inputs of `join` to strings](https://github.com/01mf02/jaq/pull/302):
    fixed by 01mf02

## License

This project is made available under the
[Mozilla Public License, v. 2.0](https://mozilla.org/MPL/2.0/).
