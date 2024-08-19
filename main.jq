# Copyright (c) 2021-2023 Thalia Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

include "ws";

def parse_src: $src | parse;
def setup_options:
  .on_eof = (
    if $on_eof == "error" or $on_eof == null then "error"
    else
      try ($on_eof | tonumber | if . != trunc then error else . end)
      catch ("Invalid value for --on-eof: \"\($on_eof)\"\n" |
        prefix_error | halt_error(2))
    end
  ) |
  .eof = $no_prompt == "true" |
  .check_clean = $check_clean == "true" |
  .check_retrieve = $check_retrieve == "true" |
  .print_pc = $print_pc == "true" |
  .filename = $filename;

def dump_error(exit_code):
  format_error
  + if .pc != null then "\n" + dump_state else "" end |
  halt_error(exit_code);
def try_or_halt(f; exit_code):
  try f
  catch
    if type == "object" and .error != null then dump_error(exit_code)
    else error end;
def try_or_continue(f):
  try f
  catch
    if type == "object" and .error != null then .
    else error end;

if $mode == "run" then
  try_or_halt(parse_src; 3) | setup_options | try_or_halt(interpret; 1)
elif $mode == "debug" then
  try_or_halt(parse_src; 3) | setup_options | debug
elif $mode == "disasm" then
  try_or_continue(parse_src) | setup_options |
  if .print_pc then disasm_pc else disasm end,
  if .error != null then dump_error(3) else empty end
elif $mode == "parse" then
  try_or_continue(parse_src) | setup_options |
  stat, if .error != null then dump_error(3) else empty end
else
  "\($mode|tojson) is not a valid mode\n" | halt_error(2)
end |
if $mode != "parse" then select(type == "string") else . end
