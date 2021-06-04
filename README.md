# wsjq — Whitespace interpreter written in jq

wsjq is an interpreter for the Whitespace programming language written
in jq.

## Usage

    wsjq [<mode>] <file>

      run    -- Run the program (default)
      debug  -- Run the program in the debugger
      disasm -- Disassemble the program
      help   -- Show usage

wsjq has been tested with jq version 1.6.

## Limitations

- Integers have 53 bits of precision because jq uses IEEE 754 float64.
- div and mod use truncated division, not floor division like Haskell.
- readc collapses CRLF to LF on Windows because `input`, strips line
  endings according to the current OS.

## License

This project is made available under the
[Mozilla Public License 2.0](https://mozilla.org/MPL/2.0/).
