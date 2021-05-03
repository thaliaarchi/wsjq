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

def disasm:
  [.prog[] | instasm | .+"\n"] | join("");
def disasmpos:
  .prog | map(instasmpos(false) + "\n") | join("");
def trace($pc; $n):
  .prog | to_entries
  | if $pc < $n then .[:$pc+$n+1]
    else .[$pc-$n:$pc+$n+1] end
  | map((.key == $pc) as $mark | .value | instasmpos($mark) + "\n")
  | join("");

def inst_error($msg; $inst):
  "Error: \($msg) at \($inst.pos)"
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

def _parse:
  def parse_num:
    match_inst(
      .arg*=2 | parse_num;           # 0 digit
      .arg*=2 | .arg+=1 | parse_num; # 1 digit
      .);                            # done
  def arg:
    .arg = 0 | match_inst(parse_num; parse_num | .arg*=-1; .); # signed
  def lbl:
    .arg = 0 | parse_num; # unsigned

  def inst($typ):
    .prog += [{typ:$typ, arg, pos}] | del(.arg) | _parse;
  def inst_label:
    if .labels[.arg|tostring] == null
    then .labels[.arg|tostring] = (.prog|length)
    else inst_error("label redefined"; {typ:"label", arg, pos}) end
    | inst("label");
  def err:
    inst_error("unrecognized instruction"; {typ:.tok, pos});

  .pos = .i | .tok = "" |
  match_inst(
    # Stack
    match_inst(
      arg | inst("push");     # SS  n push
      match_inst(
        arg | inst("copy");   # STS n copy
        err;
        arg | inst("slide")); # STL n slide
      match_inst(
        inst("dup");          # SLS   dup
        inst("swap");         # SLT   swap
        inst("drop")));       # SLL   drop
    match_inst(
      # Arithmetic
      match_inst(
        match_inst(
          inst("add");        # TSSS  add
          inst("sub");        # TSST  sub
          inst("mul"));       # TSSL  mul
        match_inst(
          inst("div");        # TSTS  div
          inst("mod");        # TSTT  mod
          err);
        err);
      # Heap
      match_inst(
        inst("store");        # TTS   store
        inst("retrieve");     # TTT   retrieve
        err);
      # I/O
      match_inst(
        match_inst(
          inst("printc");     # TLSS  printc
          inst("printi");     # TLST  printi
          err);
        match_inst(
          inst("readc");      # TLTS  readc
          inst("readi");      # TLTT  readi
          err);
        err));
    # Control flow
    match_inst(
      match_inst(
        lbl | inst_label;     # LSS l label
        lbl | inst("call");   # LST l call
        lbl | inst("jmp"));   # LSL l jmp
      match_inst(
        lbl | inst("jz");     # LTS l jz
        lbl | inst("jn");     # LTT l jn
        inst("ret"));         # LTL   ret
      match_inst(
        err;
        err;
        inst("end"));         # LLL   end
      .); # allow trailing LF
    .);

def parse:
  {
    src: explode, # program source
    i: 0,         # read offset
    pos: 0,       # instruction start offset
    tok: "",      # current token
    prog: [],     # instructions
    labels: {},   # map from label to pc
  }
  | _parse
  | del(.i, .pos, .tok);

def _interpret:
  def push($n): .s += [$n];
  def pop: .s |= .[:-1];
  def top: .s[-1];
  def top2: .s[-2];
  def jmp($l): .pc = .labels[$l|tostring];

  (.prog[.pc] // {typ:"end"}) as $inst |
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
  elif $t == "store"    then .h[top2] = top | pop | pop
  elif $t == "retrieve" then top = .h[top] // 0
  elif $t == "label"    then .
  elif $t == "call"     then .c += [.pc] | jmp($n)
  elif $t == "jmp"      then jmp($n)
  elif $t == "jz"       then if top == 0 then jmp($n) else . end | pop
  elif $t == "jn"       then if top < 0 then jmp($n) else . end | pop
  elif $t == "ret"      then .pc = .c[-1] | .c |= .[:-1]
  elif $t == "end"      then .
  elif $t == "printc"   then .out += ([top]|implode) | pop
  elif $t == "printi"   then .out += (top|tostring) | pop
  elif $t == "readc"    then .h[top] = .in[-1] | .in |= .[:-1] | pop
  elif $t == "readi"    then .h[top] = .in[-1] | .in |= .[:-1] | pop # TODO
  else inst_error("malformed inst"; $inst) end
  | if $t != "end" then _interpret else . end;

def interpret:
  . * {
    pc: 0,   # program counter
    s: [],   # data stack
    c: [],   # call stack
    h: {},   # heap
    in: "",  # stdin (TODO)
    out: "", # stdout
  }
  | _interpret;

$src | parse | interpret.out
