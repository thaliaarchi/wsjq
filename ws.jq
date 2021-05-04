# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def inststr:
  if .arg != null then "\(.typ) \(.arg)" else .typ end;
def instasm:
  if .typ == "label" then "\(.arg):"
  else "    \(inststr)" end;
def instasmpos($mark):
  "\(.pos)" + if $mark then "#" else "-" end + " \(instasm)";

def prog_entries:
  .prog | to_entries | map(.value * {pc:.key});

def disasm:
  [.prog[] | instasm | . + "\n"] | join("");
def disasmpos:
  .prog | map(instasmpos(false) + "\n") | join("");
def trace($pc; $n):
  prog_entries |
  if $pc < $n then .[:$pc+$n+1]
  else .[$pc-$n:$pc+$n+1] end |
  map(instasmpos(.pc == $pc) + "\n") | join("");

def inst_error($msg; $inst):
  "Error: \($msg) at offset \($inst.pos)"
  + if $inst != null then ": \($inst | inststr)" else "" end
  + "\n\n"
  + trace(.pc-1 // (.prog|length); 5) | halt_error(1);

def match_inst(s; t; l; eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then .tok += "S" | s # space
  elif $ch == 9  then .tok += "T" | t # tab
  elif $ch == 10 then .tok += "L" | l # lf
  elif .i >= (.src|length) then eof
  else match_inst(s; t; l; eof) end;
def match_inst(s; t; l):
  match_inst(s; t; l;
    inst_error("unexpected EOF"; {typ:.tok, pos:(.i-1)}));

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

def label_map:
  reduce (prog_entries[] | select(.typ == "label")) as $inst
    ({}; ($inst.arg|tostring) as $lbl |
      if .[$lbl] == null then .[$lbl] = $inst.pc
      else inst_error("label redefined"; $inst) end);

def interpret_inst:
  def push($n): .s += [$n];
  def pop: .s |= .[:-1];
  def top: .s[-1];
  def top2: .s[-2];
  def jmp($l): .pc = .labels[$l|tostring];
  def store($addr; $val): .h[$addr|tostring] = $val;
  def read: if .in == "" then .in = input else . end;

  if .pc < (.prog|length) then .
  else inst_error("interpreter stopped"; null) end |

  .prog[.pc] as $inst |
  $inst as {typ:$t, arg:$n} |
  .pc += 1 |
  if   $t == "push"     then push($n)
  elif $t == "dup"      then push(top)
  elif $t == "copy"     then push(.s[-$n-1])
  elif $t == "swap"     then .s = .s[:-2] + [top, top2]
  elif $t == "drop"     then pop
  elif $t == "slide"    then .s = .s[:-$n-1] + [top]
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
  elif $t == "ret"      then .pc = .c[-1] | .c |= .[:-1]
  elif $t == "end"      then .pc = (.prog|length)
  elif $t == "printc"   then ([top] | implode), pop
  elif $t == "printi"   then (top | tostring), pop
  elif $t == "readc"    then
    read | store(top; (.in|explode)[0] // 0) |
    .in |= ((explode)[1:]|implode) | pop
  elif $t == "readi"    then
    read |
    if .in | test("^\\s*[+-]?\\d+\\s*$"; "") then .
    else inst_error("invalid integer"; $inst) end |
    store(top; .in|tonumber // 0) |
    .in = "" | pop
  else inst_error("malformed instruction"; $inst) end;

def parse:
  def _parse:
    if .i < (.src|length) then parse_inst | _parse else . end;
  {
    src: explode, # program source
    i: 0,         # read offset
    pos: 0,       # instruction start offset
    tok: "",      # current token
    prog: [],     # instructions
  } |
  _parse | del(.i, .pos, .tok);

def interpret:
  def _interpret:
    if type == "string" then . # generate stream of printed strings
    elif .pc >= (.prog|length) then empty
    else interpret_inst | _interpret end;
  . * {
    labels: label_map,
    pc: 0,  # program counter
    s: [],  # data stack
    c: [],  # call stack
    h: {},  # heap
    in: "", # stdin
  } |
  _interpret;

$src | parse | interpret
