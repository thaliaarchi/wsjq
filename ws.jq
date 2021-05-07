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
def inst_asm_pos($mark):
  "\(.pos)" + if $mark then "#" else "-" end
  + if .typ != null then " \(inst_asm)" else "" end;

def prog_with_eof:
  if .i != null and .i < (.src|length) then .
  else .prog + [{pos:(.src|length)}] end;
def prog_entries:
  to_entries | map(.value * {pc:.key});

def disasm:
  [.prog[] | inst_asm + "\n"] | join("");
def disasm_pos:
  [prog_with_eof[] | inst_asm_pos(false) + "\n"] | join("");
def trace($pc; $n):
  prog_with_eof | prog_entries |
  if $pc < $n then .[:$pc+$n+1]
  else .[$pc-$n:$pc+$n+1] end |
  map(inst_asm_pos(.pc == $pc) + "\n") | join("");

def inst_error($msg; $inst):
  (.pc0 // .prog|length) as $pc |
  ($inst // .prog[$pc]) as $inst |
  "Error: \($msg)"
  + if $inst.pos != null then " at offset \($inst.pos)" else "" end
  + if $inst != null then ": \($inst | inst_str)" else "" end + "\n"
  + if .prog|length > 0 then "\n" + trace($pc; 4) else "" end |
  halt_error(1);
def inst_error($msg): inst_error($msg; null);

def assert($cond; msg; inst):
  if $cond then . else inst_error(msg; inst) end;
def assert($cond; msg): assert($cond; msg; null);

def match_inst(s; t; l; eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then .tok += "[Space]" | s
  elif $ch == 9  then .tok += "[Tab]"   | t
  elif $ch == 10 then .tok += "[LF]"    | l
  elif .i >= (.src|length) then eof
  else match_inst(s; t; l; eof) end;
def match_inst(s; t; l):
  match_inst(s; t; l;
    inst_error("unexpected EOF"; {typ:(.tok+"[EOF]"), pos:(.i-1)}));

def parse_inst:
  def parse_num:
    match_inst(
      .arg*=2 | parse_num;           # 0 digit
      .arg*=2 | .arg+=1 | parse_num; # 1 digit
      .);                            # done
  def inst_num($typ; $signed):
    .arg = 0 |
    if $signed then match_inst(parse_num; parse_num | .arg*=-1; .)
    else parse_num end |
    .prog += [{typ:$typ, arg, pos}] | del(.arg);

  def inst($typ): .prog += [{typ:$typ, pos}];
  def inst_arg($typ): inst_num($typ; true);
  def inst_lbl($typ): inst_num($typ; false);
  def inst_err: inst_error("unrecognized instruction"; {typ:.tok, pos});

  .pos = .i | .tok = "" |
  match_inst(
    # Stack
    match_inst(
      inst_arg("push");     # SS  n push
      match_inst(
        inst_arg("copy");   # STS n copy
        inst_err;
        inst_arg("slide")); # STL n slide
      match_inst(
        inst("dup");        # SLS   dup
        inst("swap");       # SLT   swap
        inst("drop")));     # SLL   drop
    match_inst(
      # Arithmetic
      match_inst(
        match_inst(
          inst("add");      # TSSS  add
          inst("sub");      # TSST  sub
          inst("mul"));     # TSSL  mul
        match_inst(
          inst("div");      # TSTS  div
          inst("mod");      # TSTT  mod
          inst_err);
        inst_err);
      # Heap
      match_inst(
        inst("store");      # TTS   store
        inst("retrieve");   # TTT   retrieve
        inst_err);
      # I/O
      match_inst(
        match_inst(
          inst("printc");   # TLSS  printc
          inst("printi");   # TLST  printi
          inst_err);
        match_inst(
          inst("readc");    # TLTS  readc
          inst("readi");    # TLTT  readi
          inst_err);
        inst_err));
    # Control flow
    match_inst(
      match_inst(
        inst_lbl("label");  # LSS l label
        inst_lbl("call");   # LST l call
        inst_lbl("jmp"));   # LSL l jmp
      match_inst(
        inst_lbl("jz");     # LTS l jz
        inst_lbl("jn");     # LTT l jn
        inst("ret"));       # LTL   ret
      match_inst(
        inst_err;
        inst_err;
        inst("end"));       # LLL   end
      .); # allow trailing LF
    .);

def parse:
  {
    src: explode, # program source
    i: 0,         # read offset
    pos: 0,       # instruction start offset
    tok: "",      # current token
    prog: [],     # instructions
  } |
  def _parse:
    if .i < (.src|length) then parse_inst | _parse else . end;
  _parse | del(.i, .pos, .tok);

def label_map:
  reduce (.prog | prog_entries[] | select(.typ == "label")) as $inst
    ({}; ($inst.arg|tostring) as $lbl |
      assert(.[$lbl] == null; "label redefined"; $inst) |
      .[$lbl] = $inst.pc);

def interpret_step:
  def assert_len($n): assert(.s|length >= $n; "stack underflow");
  def assert_ret: assert(.c|length >= 1; "call stack underflow");
  def push($n): .s += [$n];
  def pop: assert_len(1) | .s |= .[:-1];
  def at($n): assert_len($n) | .s[-$n-1];
  def top: at(0);
  def top2: at(1);
  def jmp($l): .pc = .labels[$l|tostring];
  def store($addr; $val): .h[$addr|tostring] = $val;
  def read_line:
    if .in != "" then .
    else
      . as $state |
      try (.in = input + "\n")
      catch if . == "break" then $state|inst_error("EOF") else error end
    end;

  assert(.pc < (.prog|length); "interpreter stopped") |
  .prog[.pc] as $inst |
  $inst as {typ:$t, arg:$n} |
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
  elif $t == "div"      then top2 /= top | pop
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
  elif $t == "printc"   then ([top] | implode), pop
  elif $t == "printi"   then (top | tostring), pop
  elif $t == "readc"    then
    read_line | store(top; (.in|explode)[0] // 0) |
    .in |= (explode[1:]|implode) | pop
  elif $t == "readi"    then
    read_line |
    assert(.in | test("^\\s*[+-]?\\d+\\s*$");
      "invalid integer " + (.in | rtrimstr("\n") | tojson)) |
    store(top; .in|tonumber // 0) |
    .in = "" | pop
  else inst_error("malformed instruction") end;

def interpret_init:
  . * {
    labels: label_map,
    pc: 0,  # program counter
    pc0: 0, # previous program counter
    s: [],  # data stack
    c: [],  # call stack
    h: {},  # heap
    in: "", # stdin
  };
def interpret_continue:
  if type == "string" then . # generate stream of printed strings
  elif .pc >= (.prog|length) then empty
  else interpret_step | interpret_continue end;
def interpret_next($depth):
  if $depth < 0 or type == "string" then .
  else .prog[.pc].typ as $typ | interpret_step |
    if $typ == "call" then interpret_next($depth+1)
    elif $typ == "ret" then interpret_next($depth-1)
    else interpret_next($depth) end
  end;
def interpret: interpret_init | interpret_continue;

def interpret_exit_status:
  if .prog[.pc].typ == "end"
  then "(interpreter exited cleanly)\n"
  else "(interpreter exited implicitly)\n" end;

def debug:
  def help:
    "Debugger commands:\n"
    + "  r, run        -- Launch or restart the program.\n"
    + "  c, continue   -- Continue from the current instruction.\n"
    + "  s, step       -- Execute next instruction, stepping into calls.\n"
    + "  n, next       -- Execute next instruction, stepping over calls.\n"
    + "  b, breakpoint -- Set or clear a breakpoint.\n"
    + "  q, quit       -- Quit the debugger.\n"
    + "  h, help       -- Show a list of all debugger commands.\n";
  def iscmd($cmd): . == $cmd or . == $cmd[:1];
  def breakpoint: "Not implemented\n"; # TODO
  def _debug:
    "(wsjq) ",
    ((try input
      catch if . == "break" then "q" else error end) as $cmd |
    (if $cmd == "" then .prev_cmd else $cmd end) as $cmd |
    if   $cmd|iscmd("run")        then interpret
    elif $cmd|iscmd("continue")   then interpret_continue
    elif $cmd|iscmd("step")       then "step\n", interpret_step
    elif $cmd|iscmd("next")       then "next\n", interpret_next(0)
    elif $cmd|iscmd("breakpoint") then breakpoint
    elif $cmd|iscmd("quit")       then .
    elif $cmd|iscmd("help")       then help
    elif $cmd == ""               then .
    else "\($cmd|tojson) is not a valid command.\n", . end |
    if type == "string" or $cmd[:1] == "q" then .
    else _debug end);
  . * {
    breakpoints: {},
    prev_cmd: "",
  } |
  interpret_init | _debug;

$src | parse | debug
