# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def color($c): "\u001b[\($c)m\(.)\u001b[0m";
def black:   color(30); def bright_black:   color(90);
def red:     color(31); def bright_red:     color(91);
def green:   color(32); def bright_green:   color(92);
def yellow:  color(33); def bright_yellow:  color(93);
def blue:    color(34); def bright_blue:    color(94);
def magenta: color(35); def bright_magenta: color(95);
def cyan:    color(36); def bright_cyan:    color(96);
def white:   color(37); def bright_white:   color(97);

def inst_str:
  if .arg != null then "\(.typ) \(.arg)" else .typ end;
def inst_asm:
  if .typ == "label" then "\(.arg):"
  else "    \(inst_str)" end;
def inst_asm_pos($mark; $w):
  if $mark then "\(.pos)#"|yellow else "\(.pos)-" end +
  if .typ == null then ""
  else
    ([$w - (.pos|tostring|length), 0] | max + 1) as $w |
    " " * $w + inst_asm
  end;

def inst_line($pos):
  .lines |
  bsearch($pos) as $i |
  (if $i < 0 then -(2+$i) else $i end) as $i |
  "\($i+1):\($pos-.[$i]+1)";

def prog_with_eof:
  if .i != null and .i < (.src|length) then .
  else .prog + [{pos:(.src|length)}] end;
def prog_entries:
  to_entries | map(.value * {pc:.key});

def disasm:
  [.prog[] | inst_asm + "\n"] | join("");
def _disasm_pos(mark):
  (last.pos|tostring|length) as $w |
  map(inst_asm_pos(mark; $w) + "\n") | join("");
def disasm_pos(mark):
  prog_with_eof | prog_entries | _disasm_pos(mark);
def disasm_pos: disasm_pos(false);
def trace($pc; $n_before; $n_after):
  prog_with_eof | prog_entries |
  if $pc < $n_before then .[:$pc+$n_after+1]
  else .[$pc-$n_before:$pc+$n_after+1] end |
  _disasm_pos(.pc == $pc);
def trace($pc; $n): trace($pc; $n; $n);
def dump_state:
  def stack: .s | join(", ");
  def calls: [.c[] as $c | .prog[$c-1].arg] | join(", ");
  def heap:
    [(.h | keys | sort_by(tonumber)[]) as $k | "\($k):\(.h[$k])"] |
    join(", ");
  "Stack: [\(stack)]\n" +
  "Calls: [\(calls)]\n" +
  "Heap:  {\(heap)}\n";

def inst_error($msg; $inst; $pc):
  ($pc // .pc0 // .prog|length) as $pc |
  ($inst // .prog[$pc]) as $inst |
  ("Error:"|bright_red) + " " + $msg
  + if $inst.pos != null then
      " at \(inst_line($inst.pos)) (offset \($inst.pos))" else "" end
  + if $inst != null then ": \($inst | inst_str)" else "" end + "\n"
  + if .prog|length > 0 then "\n" + trace($pc; 4) else "" end
  + if .pc!=null then "\n" + dump_state else "" end |
  halt_error(1);
def inst_error($msg; $inst): inst_error($msg; $inst; $inst.pc);
def inst_error($msg): inst_error($msg; null);

def assert($cond; msg; inst):
  if $cond then . else inst_error(msg; inst) end;
def assert($cond; msg): assert($cond; msg; null);

def match_char(s; t; l; eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then .tok += "[Space]" | s
  elif $ch == 9  then .tok += "[Tab]"   | t
  elif $ch == 10 then .tok += "[LF]"    | .lines += [.i] | l
  elif .i >= (.src|length) then eof
  else match_char(s; t; l; eof) end;
def match_char(s; t; l):
  match_char(s; t; l;
    inst_error("unexpected EOF"; {typ:(.tok+"[EOF]"), pos:(.i-1)}));

def parse_inst:
  def parse_num:
    match_char(
      .n*=2 | parse_num;         # 0 digit
      .n*=2 | .n+=1 | parse_num; # 1 digit
      .);                        # done
  def parse_lbl:
    def digit($d): .n*=2 | .n+=$d | .l+=[$d] | parse_lbl;
    match_char(digit(0); digit(1); .);
  def lbl_str:
    .n as $n | .l |
    if length%8 == 0 and length > 0 then
      [range(0;length;8) as $i |
        reduce .[$i:$i+8][] as $d (0; .*2 + $d)] |
      # Label is visible ASCII and doesn't start with %
      if all(33 <= . and . <= 126) and .[0] != 37
      then implode else $n end
    else $n end |
    if type == "number" then "%\(.)" else . end;

  def inst($typ): .prog += [{typ:$typ, pos}];
  def inst_num($typ):
    .n = 0 | match_char(parse_num; parse_num | .n*=-1; .) |
    .prog += [{typ:$typ, arg:.n, pos}] | del(.n);
  def inst_lbl($typ):
    .n = 0 | .l = [] | parse_lbl |
    .prog += [{typ:$typ, arg:lbl_str, pos}] | del(.n, .l);
  def inst_err: inst_error("unrecognized instruction"; {typ:.tok, pos});

  .pos = .i | .tok = "" |
  match_char(
    # Stack
    match_char(
      inst_num("push");     # SS  n push
      match_char(
        inst_num("copy");   # STS n copy
        inst_err;
        inst_num("slide")); # STL n slide
      match_char(
        inst("dup");        # SLS   dup
        inst("swap");       # SLT   swap
        inst("drop")));     # SLL   drop
    match_char(
      # Arithmetic
      match_char(
        match_char(
          inst("add");      # TSSS  add
          inst("sub");      # TSST  sub
          inst("mul"));     # TSSL  mul
        match_char(
          inst("div");      # TSTS  div
          inst("mod");      # TSTT  mod
          inst_err);
        inst_err);
      # Heap
      match_char(
        inst("store");      # TTS   store
        inst("retrieve");   # TTT   retrieve
        inst_err);
      # I/O
      match_char(
        match_char(
          inst("printc");   # TLSS  printc
          inst("printi");   # TLST  printi
          inst_err);
        match_char(
          inst("readc");    # TLTS  readc
          inst("readi");    # TLTT  readi
          inst_err);
        inst_err));
    # Control flow
    match_char(
      match_char(
        inst_lbl("label");  # LSS l label
        inst_lbl("call");   # LST l call
        inst_lbl("jmp"));   # LSL l jmp
      match_char(
        inst_lbl("jz");     # LTS l jz
        inst_lbl("jn");     # LTT l jn
        inst("ret"));       # LTL   ret
      match_char(
        inst_err;
        inst_err;
        inst("end"));       # LLL   end
      .); # allow trailing LF
    .);

def label_map:
  . as $state |
  reduce (.prog | prog_entries[] | select(.typ == "label")) as $inst
    ({}; . as $labels |($inst.arg|tostring) as $lbl |
      $state | assert($labels[$lbl] == null; "label redefined"; $inst) |
      $labels | .[$lbl] = $inst.pc);

def parse:
  {
    src: explode, # program source
    i: 0,         # read offset
    pos: 0,       # instruction start offset
    tok: "",      # current token
    lines: [0],   # offsets of lines
    prog: [],     # instructions
  } |
  def _parse:
    if .i < (.src|length) then parse_inst | _parse else . end;
  _parse |
  del(.i, .pos, .tok) |
  .labels = label_map;

def interpret_step(format_print; read_prefix):
  def assert_len($n): assert(.s|length >= $n; "stack underflow");
  def assert_ret: assert(.c|length >= 1; "call stack underflow");
  def push($n): .s += [$n];
  def pop: assert_len(1) | .s |= .[:-1];
  def at($n): assert_len($n+1) | .s[-$n-1];
  def top: at(0);
  def top2: at(1);
  def store($addr; $val): .h[$addr|tostring] = $val;
  def jmp($l):
    assert(.labels|has($l); "undefined label") | .pc = .labels[$l];
  def read(process):
    assert_len(1) |
    if .in != "" then process
    else
      . as $state |
      try (.in = input + "\n" | process)
      catch
        if . != "break" then error
        elif .on_eof|type == "number" then store(top; .on_eof) | pop
        else $state|inst_error("EOF") end
    end;
  def readc:
    store(top; (.in|explode)[0]) | pop | .in |= .[1:];
  def readi:
    assert(.in | test("^\\s*[+-]?\\d+\\s*$");
      "invalid integer " + (.in | rtrimstr("\n") | tojson)) |
    store(top; .in|tonumber) | pop | .in = "";

  assert(.pc < (.prog|length); "interpreter stopped") |
  .prog[.pc] as $inst | $inst as {typ:$t, arg:$n} |
  .pc0 = .pc | .pc += 1 |
  if   $t == "push"     then push($n)
  elif $t == "dup"      then push(top)
  elif $t == "copy"     then push(at($n))
  elif $t == "swap"     then .s = .s[:-2] + [top, top2]
  elif $t == "drop"     then pop
  elif $t == "slide"    then assert_len($n) | .s = .s[:-$n-1] + [top]
  elif $t == "add"      then top2 += top | pop
  elif $t == "sub"      then top2 -= top | pop
  elif $t == "mul"      then top2 *= top | pop
  elif $t == "div"      then top2 = (top2 / top | trunc) | pop
  elif $t == "mod"      then top2 %= top | pop
  elif $t == "store"    then store(top2; top) | pop | pop
  elif $t == "retrieve" then top = (.h[top|tostring] // 0)
  elif $t == "label"    then .
  elif $t == "call"     then .c += [.pc] | jmp($n)
  elif $t == "jmp"      then jmp($n)
  elif $t == "jz"       then if top == 0 then jmp($n) else . end | pop
  elif $t == "jn"       then if top < 0 then jmp($n) else . end | pop
  elif $t == "ret"      then assert_ret | .pc = .c[-1] | .c |= .[:-1]
  elif $t == "end"      then .pc = (.prog|length)
  elif $t == "printc"   then ([top] | implode | format_print), pop
  elif $t == "printi"   then (top | format_print), pop
  elif $t == "readc"    then read_prefix, read(readc)
  elif $t == "readi"    then read_prefix, read(readi)
  else inst_error("malformed instruction") end;
def interpret_step: interpret_step(tostring; empty);

def interpret_step_debug:
  def print_exit_status:
    if type != "object" or .pc < (.prog|length) then empty
    else
      if .prog[.pc0].typ == "end"
      then "[program exited cleanly]\n"|green
      else "[program exited implicitly]\n"|red end
    end;
  if .pc >= (.prog|length) then ("[interpreter stopped]\n"|red), .
  else
    .moved = true |
    interpret_step(
      ("print>"|bright_cyan) + " \(tojson)\n";
      "read<"|bright_cyan + " ") |
    print_exit_status, .
  end;

def interpret_init:
  . + {
    pc: 0,  # program counter
    pc0: 0, # previous program counter
    s: [],  # data stack
    c: [],  # call stack
    h: {},  # heap
    in: "", # stdin
  };

def interpret_continue:
  # If parameters are recursively passed to interpret_contine, jq does
  # not detect the tail call and perform tail-call optimization.
  if type != "object" or .pc >= (.prog|length) then .
  else interpret_step | interpret_continue end;
def interpret_continue_debug:
  if type != "object" or .pc >= (.prog|length) then .
  elif .breaks[.pc|tostring] then ("[stopped at breakpoint]\n"|red), .
  else interpret_step_debug | interpret_continue_debug end;

def _interpret_next($verbose; $depth):
  if $verbose then interpret_step_debug else interpret_step end |
  if type != "object" or .pc >= (.prog|length) then .
  elif .breaks[.pc|tostring] then ("[stopped at breakpoint]\n"|red), .
  else
    .prog[.pc0].typ as $typ |
    (if $typ == "call" then $depth+1
      elif $typ == "ret" then $depth-1
      else $depth end) as $depth |
    if $depth > 0 then _interpret_next($verbose; $depth) else . end
  end;
def interpret_next: _interpret_next(false; 0);
def interpret_next_debug: _interpret_next(true; 0);

def interpret: interpret_init | interpret_continue;

def debug:
  def help:
    "Debugger commands:\n"
    + "  r, run         -- Run or restart the program from the start\n"
    + "  c, continue    -- Continue from the current instruction\n"
    + "  s, step        -- Execute next instruction, stepping into calls\n"
    + "  n, next        -- Execute next instruction, stepping over calls\n"
    + "  b, breakpoint  -- Set or clear a breakpoint\n"
    + "  d, disasm      -- Disassemble program\n"
    + "  p, print       -- Dump the data stack, call stack, and heap\n"
    + "  q, quit        -- Quit the debugger\n"
    + "  h, help        -- Show a list of all debugger commands\n";
  def iscmd($cmd): . == $cmd or . == $cmd[:1];
  def print_error: ("Error:"|bright_red) + " \(.)\n";
  def run:
    if .pc > 0 then ("[interpreter restarted]\n"|green), interpret_init
    else interpret_init | interpret_continue_debug end;
  def breakpoint($locs):
    if $locs|length == 0 then
      .prog[.breaks | keys[] | tonumber] | inst_asm_pos(false; 0) + "\n"
    else
      $locs[] as $l |
      if .breaks|has($l) then .breaks[$l] |= not
      elif .labels|has($l) then .breaks[.labels[$l]|tostring] = true
      else "Label not found: \($l)"|print_error end
    end;
  def _debug:
    if .moved and .pc < (.prog|length) then trace(.pc; 0; 3)
    else empty end,
    ("(wsjq)"|bright_black+" "),
    ((try input
      catch if . == "break" then "q" else error end) as $cmd |
    ($cmd | gsub("^\\s+|\\s+$"; "")) as $cmd |
    (if $cmd == "" then .cmd else $cmd end) as $cmd |
    ($cmd | split("\\s+"; "")) as $words |
    ($words[0] // "") as $c | $words[1:] as $args |
    .moved = false |
    if   $c == ""               then .
    elif $c|iscmd("run")        then .cmd = "" | run
    elif $c|iscmd("continue")   then .cmd = "" | interpret_continue_debug
    elif $c|iscmd("step")       then .cmd = $cmd | interpret_step_debug
    elif $c|iscmd("next")       then .cmd = $cmd | interpret_next_debug
    elif $c|iscmd("disasm")     then .pc as $pc | disasm_pos(.pc == $pc), .
    elif $c|iscmd("breakpoint") then breakpoint($args), .
    elif $c|iscmd("print")      then dump_state, .
    elif $c|iscmd("quit")       then ""
    elif $c|iscmd("help")       then help, .
    else ("\($cmd|tojson) is not a valid command"|print_error), . end |
    if type != "object" then .
    else _debug end);
  . + {
    cmd: "",     # previous debug command
    breaks: {},  # key: breakpoint pc, value: boolean status
    moved: true, # whether the last command moved pc
  } |
  interpret_init | _debug;
