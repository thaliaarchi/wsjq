# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def inst_str:
  if .arg != null then "\(.typ) \(.arg)" else .typ end;
def inst_asm:
  if .typ == "label" then "\(.arg):"
  else "    \(inst_str)" end;
def inst_asm_pos($mark; $w):
  "\(.pos)" + if $mark then "#" else "-" end
  + if .typ == null then ""
    else " " * ($w+1 - (.pos|tostring|length)) + inst_asm end;

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
def trace($pc; $n):
  prog_with_eof | prog_entries |
  if $pc < $n then .[:$pc+$n+1]
  else .[$pc-$n:$pc+$n+1] end |
  _disasm_pos(.pc == $pc);
def dump_state:
  ([.c[] as $c | .prog[$c-1].arg] | join(",")) as $c |
  "Stack: \(.s)\n" +
  "Calls: [\($c)]\n" +
  "Heap:  \(.h)\n";

def inst_error($msg; $inst; $pc):
  ($pc // .pc0 // .prog|length) as $pc |
  ($inst // .prog[$pc]) as $inst |
  "Error: \($msg)"
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
      # visible ASCII that doesn't start with %
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
  reduce (.prog | prog_entries[] | select(.typ == "label")) as $inst
    ({}; ($inst.arg|tostring) as $lbl |
      assert(.[$lbl] == null; "label redefined"; $inst) |
      .[$lbl] = $inst.pc);

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

def interpret_step(before; format_print; read_prefix):
  def assert_len($n): assert(.s|length >= $n; "stack underflow");
  def assert_ret: assert(.c|length >= 1; "call stack underflow");
  def push($n): .s += [$n];
  def pop: assert_len(1) | .s |= .[:-1];
  def at($n): assert_len($n) | .s[-$n-1];
  def top: at(0);
  def top2: at(1);
  def store($addr; $val): .h[$addr|tostring] = $val;
  def jmp($l):
    assert(.labels|has($l); "undefined label") | .pc = .labels[$l];
  def read_line:
    assert_len(1) |
    if .in != "" then .
    else
      . as $state |
      try (.in = input + "\n")
      catch if . == "break" then $state|inst_error("EOF") else error end
    end;

  assert(.pc < (.prog|length); "interpreter stopped") |
  before,
  .prog[.pc] as $inst | $inst as {typ:$t, arg:$n} |
  .pc0 = .pc | .pc += 1 |
  (if  $t == "push"     then push($n)
  elif $t == "dup"      then push(top)
  elif $t == "copy"     then push(at($n))
  elif $t == "swap"     then .s = .s[:-2] + [top, top2]
  elif $t == "drop"     then pop
  elif $t == "slide"    then assert_len($n) | .s = .s[:-$n-1] + [top]
  elif $t == "add"      then top2 += top | pop
  elif $t == "sub"      then top2 -= top | pop
  elif $t == "mul"      then top2 *= top | pop
  elif $t == "div"      then top2 = (top2 / top | floor) | pop
  elif $t == "mod"      then top2 %= top | pop
  elif $t == "store"    then store(top2; top) | pop | pop
  elif $t == "retrieve" then top = .h[top|tostring] // 0
  elif $t == "label"    then .
  elif $t == "call"     then .c += [.pc] | jmp($n)
  elif $t == "jmp"      then jmp($n)
  elif $t == "jz"       then if top == 0 then jmp($n) else . end | pop
  elif $t == "jn"       then if top < 0 then jmp($n) else . end | pop
  elif $t == "ret"      then assert_ret | .pc = .c[-1] | .c |= .[:-1]
  elif $t == "end"      then .pc = (.prog|length)
  elif $t == "printc"   then ([top] | implode | format_print), pop
  elif $t == "printi"   then (top | format_print), pop
  elif $t == "readc"    then
    read_prefix, (read_line |
    store(top; (.in|explode)[0]) | pop | .in |= .[1:])
  elif $t == "readi"    then
    read_prefix, (read_line |
    assert(.in | test("^\\s*[+-]?\\d+\\s*$");
      "invalid integer " + (.in | rtrimstr("\n") | tojson)) |
    store(top; .in|tonumber) | pop | .in = "")
  else inst_error("malformed instruction") end);
def interpret_step: interpret_step(empty; tostring; empty);

def interpret_step_debug:
  def print_exit_status:
    if type != "object" or .pc < (.prog|length) then empty
    else
      if .prog[.pc0].typ == "end"
      then "[program exited cleanly]\n"
      else "[program exited implicitly]\n" end
    end;

  if .pc >= (.prog|length) then "[interpreter stopped]\n", .
  else
    interpret_step(
      (.src|length|tostring|length) as $w |
      .prog[.pc] | inst_asm_pos(false; $w) + "\n";
      "print> \(tojson)\n";
      "read< ") |
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
  # Continue loops are duplicated to remove recursive parameters and
  # enable tail-call optimization.
  if type != "object" or .pc >= (.prog|length) then .
  else interpret_step | interpret_continue end;
def interpret_continue_debug:
  if type != "object" or .pc >= (.prog|length) then .
  else interpret_step_debug | interpret_continue_debug end;

def _interpret_next($verbose; $depth):
  if $verbose then interpret_step_debug else interpret_step end |
  if type != "object" or .pc >= (.prog|length) then .
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
    # + "  b, breakpoint  -- Set or clear a breakpoint\n"
    + "  d, disassemble -- Disassemble program\n"
    + "  p, print       -- Dump the data stack, call stack, and heap\n"
    + "  q, quit        -- Quit the debugger\n"
    + "  h, help        -- Show a list of all debugger commands\n";
  def run:
    if .pc > 0 then "[interpreter restarted]\n", interpret_init
    else interpret_init | interpret_continue_debug end;
  def iscmd($cmd): . == $cmd or . == $cmd[:1];
  def _debug:
    "(wsjq) ",
    ((try input
      catch if . == "break" then "q" else error end) as $cmd |
    (if $cmd == "" then .cmd0 else $cmd end) as $cmd |
    .cmd0 = "" |
    if   $cmd|iscmd("run")         then run
    elif $cmd|iscmd("continue")    then interpret_continue
    elif $cmd|iscmd("step")        then .cmd0 = $cmd | interpret_step_debug
    elif $cmd|iscmd("next")        then .cmd0 = $cmd | interpret_next_debug
    elif $cmd|iscmd("disassemble") then .pc as $pc | disasm_pos(.pc == $pc), .
    # elif $cmd|iscmd("breakpoint")  then breakpoint
    elif $cmd|iscmd("print")       then dump_state, .
    elif $cmd|iscmd("quit")        then .
    elif $cmd|iscmd("help")        then help
    elif $cmd == ""                then .
    else "\($cmd|tojson) is not a valid command\n", . end |
    if type != "object" or $cmd[:1] == "q" then .
    else _debug end);
  . + {
    cmd: "",  # debug command
    cmd0: "", # previous debug command
    # breakpoints: {},
  } |
  interpret_init | _debug;
