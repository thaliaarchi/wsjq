# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def inststr:
  if .arg != null then "\(.typ) \(.arg)" else .typ end;
def instasm:
  if .typ == "label" then "\(.arg):"
  elif .typ == "EOF" then "; EOF"
  else "    \(inststr)" end;
def instasmpos($mark):
  "\(.pos)" + (if $mark then ":" else "-" end) + " \(instasm)";

def disasm:
  [.prog[] | instasm | .+"\n"] | join("");
def disasmpos:
  .prog | map(instasmpos(false) + "\n") | join("");
def trace($pc; $n):
  [.prog[], {typ:"EOF", pos:.src|length}]
  | to_entries
  | if $pc < $n then .[:$pc+$n+1]
    else .[$pc-$n:$pc+$n+1] end
  | map(.value | instasmpos(.key == $pc) + "\n")
  | join("");

def parse_error($msg; $pos; $inst):
  "Parse error: \($msg) at \($pos)"
  + (if $inst != null then $inst | inststr else "" end)
  + "\n\n"
  + trace(.prog|length; 5) | halt_error(1);
def inst_error($msg; $inst):
  parse_error($msg; $inst.pos; $inst);
def badinst($typ):
  inst_error("unrecognized instruction"; {typ:$typ, pos});

def matchinst(s; t; l; $can_eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then s
  elif $ch == 9  then t
  elif $ch == 10 then l
  elif .i < (.src|length) then matchinst(s; t; l; $can_eof)
  elif $can_eof then .
  else parse_error("unexpected EOF"; .i-1; null) end;
def matchinst(s; t; l): matchinst(s; t; l; false);

def parsearg:
  def arg:
    matchinst(
      .arg*=2 | arg;
      .arg*=2 | .arg+=1 | arg;
      .);
  .arg=0 | matchinst(arg; arg | .arg*=-1; .);

def inst($typ):
  .prog[.prog|length] = {typ:$typ, pos};
def instarg($typ):
  parsearg |
  .prog[.prog|length] = {typ:$typ, arg, pos};
def instlabel:
  instarg("label") |
  if .labels[.arg|tostring] == null
  then .labels[.arg|tostring] = (.prog|length)
  else inst_error("label redefined"; {typ:"label", arg, pos}) end;

def parse:
  .pos = .i |
  matchinst(
    # Stack
    matchinst(
      instarg("push") | parse;     # SS  n push
      matchinst(
        instarg("copy") | parse;   # STS n copy
        badinst("STT");
        instarg("slide") | parse); # STL n slide
      matchinst(
        inst("dup") | parse;       # SLS   dup
        inst("swap") | parse;      # SLT   swap
        inst("drop") | parse));    # SLL   drop
    matchinst(
      # Arithmetic
      matchinst(
        matchinst(
          inst("add") | parse;     # TSSS  add
          inst("sub") | parse;     # TSST  sub
          inst("mul") | parse);    # TSSL  mul
        matchinst(
          inst("div") | parse;     # TSTS  div
          inst("mod") | parse;     # TSTT  mod
          badinst("TSTL"));
        badinst("TSL"));
      # Heap
      matchinst(
        inst("store") | parse;     # TTS   store
        inst("retrieve") | parse;  # TTT   retrieve
        badinst("TTL"));
      # I/O
      matchinst(
        matchinst(
          inst("printc") | parse;  # TLSS  printc
          inst("printi") | parse;  # TLST  printi
          badinst("TLSL"));
        matchinst(
          inst("readc") | parse;   # TLTS  readc
          inst("readi") | parse;   # TLTT  readi
          badinst("TLTL"));
        badinst("TLL")));
    # Control flow
    matchinst(
      matchinst(
        instlabel | parse;         # LSS l label
        instarg("call") | parse;   # LST l call
        instarg("jmp") | parse);   # LSL l jmp
      matchinst(
        instarg("jz") | parse;     # LTS l jz
        instarg("jn") | parse;     # LTT l jn
        inst("ret") | parse);      # LTL   ret
      matchinst(
        badinst("LLS");
        badinst("LLT");
        inst("end") | parse);      # LLL   end
      true); # allow trailing LF
  true)
  | del(.i) | del(.pos) | del(.arg);

{
  src: $src|explode,
  i: 0,
  prog: [],
  labels: {},
}
| parse
| disasmpos
