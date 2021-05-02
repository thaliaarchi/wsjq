# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def matchinst(s; t; l; can_eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then s
  elif $ch == 9  then t
  elif $ch == 10 then l
  elif .i < (.src|length) then matchinst(s; t; l; can_eof)
  elif can_eof then .
  else "Error: unexpected EOF at \(.i)\n" | halt_error end;
def matchinst(s; t; l): matchinst(s; t; l; false);

def disasm:
  [.prog[]
  | if .name == "label" then "\(.arg):"
    elif .arg != null then "    \(.name) \(.arg)"
    else "    \(.name)" end + "\n"]
  | join("");

def inst($name):
  .prog[.prog|length] = {name:$name};

def instn($name):
  def num:
    matchinst(
      .n*=2 | num;
      .n*=2 | .n+=1 | num;
      .);
  .n=0 | matchinst(num; num | .n*=-1; .) |
  .prog[.prog|length] = {name:$name, arg:.n} |
  del(.n);

def badinst($name):
  "Error: unrecognized instruction "
  + "\($name) at \(.i - ($name|length) + 1)\n\n"
  + disasm
  | halt_error(1);

def parse:
  matchinst(
    # Stack
    matchinst(
      instn("push") | parse;      # SS  n push
      matchinst(
        instn("copy") | parse;    # STS n copy
        badinst("STT");
        instn("slide") | parse);  # STL n slide
      matchinst(
        inst("dup") | parse;      # SLS   dup
        inst("swap") | parse;     # SLT   swap
        inst("drop") | parse));   # SLL   drop
    matchinst(
      # Arithmetic
      matchinst(
        matchinst(
          inst("add") | parse;    # TSSS  add
          inst("sub") | parse;    # TSST  sub
          inst("mul") | parse);   # TSSL  mul
        matchinst(
          inst("div") | parse;    # TSTS  div
          inst("mod") | parse;    # TSTT  mod
          badinst("TSTL"));
        badinst("TSL"));
      # Heap
      matchinst(
        inst("store") | parse;    # TTS   store
        inst("retrieve") | parse; # TTT   retrieve
        badinst("TTL"));
      # I/O
      matchinst(
        matchinst(
          inst("printc") | parse; # TLSS  printc
          inst("printi") | parse; # TLST  printi
          badinst("TLSL"));
        matchinst(
          inst("readc") | parse;  # TLTS  readc
          inst("readi") | parse;  # TLTT  readi
          badinst("TLTL"));
        badinst("TLL")));
    # Control flow
    matchinst(
      matchinst(
        instn("label") | parse;   # LSS l label
        instn("call") | parse;    # LST l call
        instn("jmp") | parse);    # LSL l jmp
      matchinst(
        instn("jz") | parse;      # LTS l jz
        instn("jn") | parse;      # LTT l jn
        inst("ret") | parse);     # LTL   ret
      matchinst(
        badinst("LLS");
        badinst("LLT");
        inst("end") | parse);  # LLL   end
      true); # allow trailing LF
  true)
  | del(.src) | del(.i);

{src:$src|explode, i:0, prog:[]} | parse | disasm | halt_error(0)
