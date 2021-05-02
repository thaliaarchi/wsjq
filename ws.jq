# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def space: 32;
def tab:   9;
def lf:    10;

def match(s; t; l; can_eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == space then s
  elif $ch == tab   then t
  elif $ch == lf    then l
  elif .i < (.src|length) then match(s; t; l; can_eof)
  elif can_eof then .
  else "Error: unexpected EOF at \(.i)\n" | halt_error end;
def match(s; t; l): match(s; t; l; false);

def inst($name):
  .insts[.insts|length] = {name:$name};

def instn($name):
  .insts[.insts|length] = {name:$name, arg:0}; # TODO parse arg

def badinst($name):
  "Error: unrecognized instruction \($name) at \(.i - ($name|length) + 1)\n" | halt_error(1);

def parse:
  match(
    # Stack
    match(
      instn("push") | parse;      # SS  n push
      match(
        instn("copy") | parse;    # STS n copy
        badinst("STT");
        instn("slide") | parse);  # STL n slide
      match(
        inst("dup") | parse;      # SLS   dup
        inst("swap") | parse;     # SLT   swap
        inst("drop") | parse));   # SLL   drop
    match(
      # Arithmetic
      match(
        match(
          inst("add") | parse;    # TSSS  add
          inst("sub") | parse;    # TSST  sub
          inst("mul") | parse);   # TSSL  mul
        match(
          inst("div") | parse;    # TSTS  div
          inst("mod") | parse;    # TSTT  mod
          badinst("TSTN"));
        badinst("TSN"));
      # Heap
      match(
        inst("store") | parse;    # TTS   store
        inst("retrieve") | parse; # TTT   retrieve
        badinst("TTN"));
      # I/O
      match(
        match(
          inst("printc") | parse; # TLSS  printc
          inst("printi") | parse; # TLST  printi
          badinst("TLSN"));
        match(
          inst("readc") | parse;  # TLTS  readc
          inst("readi") | parse;  # TLTT  readi
          badinst("TLTN"))));
    # Control flow
    match(
      match(
        instn("label") | parse;   # LSS l label
        instn("call") | parse;    # LST l call
        instn("jmp") | parse);    # LSL l jmp
      match(
        instn("jz") | parse;      # LTS l jz
        instn("jn") | parse;      # LTT l jn
        inst("ret") | parse);     # LTL   ret
      match(
        badinst("LS");
        badinst("LT");
        match(
          badinst("LLS");
          badinst("LLT");
          inst("end") | parse));  # LLL   end
      true); # allow trailing LF
  true);

{src:" \n \t   "|explode, i:0, insts:[]} | parse
