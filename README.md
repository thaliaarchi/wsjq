# wsjq — Whitespace interpreter in jq

wsjq is an interpreter and debugger for the Whitespace programming
language, written in jq.

## Usage

    usage: wsjq [<mode>] [-h|--help] [-e|--on-eof=<behavior>] [-c|--check-clean] <file>

    Modes:
      run    -- Run the program (default)
      debug  -- Run the program in the debugger
      disasm -- Disassemble the program

    Options:
      -e, --on-eof=<behavior>
          Set EOF-handling behavior, which can be "error" (like wspace) or
          an integer (e.g. 0 or -1).

      -c, --check-clean
          Assert that the stack is empty and that the program explicitly at
          the end of execution.

      -h, --help
          Show usage.

wsjq has been tested with jq version 1.6.

## Real-time I/O

wsjq prints to stdout by generating a stream of strings that are
immediately emitted, rather than by appending to a buffer. wsjq sets the
main input source to be stdin so that user input is streamed
line-by-line with `input`. The contents of the Whitespace source file
are then bound to the `$src` variable with `--rawfile`.

## Limitations

- Integers have 53 bits of precision because jq uses IEEE 754 float64.
- div and mod use truncated division, not floor division like Haskell.
- readc collapses CRLF to LF on Windows because `input` strips line
  endings according to the current OS.

## See also

[bf.jq](https://github.com/itchyny/brainfuck/blob/master/bf.jq) by itchyny
([blog post](https://itchyny.medium.com/json-formatter-written-in-jq-b716c281afd7))
and [bf.jq](https://github.com/MakeNowJust/bf.jq/blob/master/bf.jq) by
MakeNowJust are Brainfuck interpreters written in jq, though neither
support the `,` read operator and both display `.` output on program
termination.

The jq standard library filters defined in
[builtin.jq](https://github.com/stedolan/jq/blob/master/src/builtin.jq)
are a helpful reference for learning advanced usage.

## License

This project is made available under the
[Mozilla Public License 2.0](https://mozilla.org/MPL/2.0/).
