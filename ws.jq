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
  "\(.pos)" + (if $mark then "#" else "-" end) + " \(instasm)";

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

def parse_error($msg; $pos; $inst):
  "Parse error: \($msg) at \($pos)"
  + (if $inst != null then ": \($inst | inststr)" else "" end)
  + "\n\n"
  + trace(.prog|length; 5) | halt_error(1);
def inst_error($msg; $inst):
  parse_error($msg; $inst.pos; $inst);

def match_inst(s; t; l; eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then .tok += "S" | s
  elif $ch == 9  then .tok += "T" | t
  elif $ch == 10 then .tok += "L" | l
  elif .i >= (.src|length) then eof
  else match_inst(s; t; l; eof) end;
def match_inst(s; t; l):
  match_inst(s; t; l;
    parse_error("unexpected EOF"; .i-1; {typ:.tok, pos}));

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
    src: explode,
    i: 0,
    tok: "",
    prog: [],
    labels: {},
  }
  | _parse
  | del(.i, .pos, .tok);

$src | parse | disasmpos
