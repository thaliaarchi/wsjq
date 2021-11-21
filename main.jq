# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

include "ws";

(if $on_eof == "error" or $on_eof == null then "error"
  else
    try ($on_eof | tonumber | if . != trunc then error else . end)
    catch ("Invalid value for --on-eof: \"\($on_eof)\"\n" |
      prefix_error | halt_error(2))
  end) as $on_eof |
$src | parse |
.on_eof = $on_eof |
.check_clean = $check_clean == "true" |
if   $mode == "run"    then interpret
elif $mode == "debug"  then debug
elif $mode == "disasm" then disasm_pc
else "\($mode|tojson) is not a valid mode\n" | halt_error(2)
end |
select(type == "string")
