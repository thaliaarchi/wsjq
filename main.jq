# Copyright (c) 2021 Andrew Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

include "ws";

$src | parse |
if   $mode == "run"    then interpret
elif $mode == "debug"  then debug
elif $mode == "disasm" then disasm_pc
else "\($mode|tojson) is not a valid mode\n" | halt_error(2)
end |
select(type == "string")
