# Copyright (c) 2021-2022 Thalia Archibald
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

def color($c): "\u001b[\($c)m\(.)\u001b[0m";
def black:   color(30); def bright_black:   color(90);
def red:     color(31); def bright_red:     color(91);
def green:   color(32); def bright_green:   color(92);
def yellow:  color(33); def bright_yellow:  color(93);
def blue:    color(34); def bright_blue:    color(94);
def magenta: color(35); def bright_magenta: color(95);
def cyan:    color(36); def bright_cyan:    color(96);
def white:   color(37); def bright_white:   color(97);

def inst_str:
  if .arg != null then "\(.typ) \(.arg)" else .typ end;
def inst_asm:
  if .typ == "label" then "\(.arg):"
  else "    \(inst_str)" end;
def inst_asm_pc($pc; $breaks; $width):
  if .pc == $pc then "\(.pc)#"|yellow
  elif .pc|tostring | in($breaks) then
    if $breaks[.pc|tostring] then "\(.pc)*"|red else "\(.pc)*" end
  else "\(.pc)-" end +
  if .typ == null then ""
  else
    ([$width - (.pc|tostring|length), 0] | max + 1) as $width |
    " " * $width + inst_asm
  end + "\n";

def inst_line($pos):
  .lines |
  bsearch($pos) as $i |
  (if $i < 0 then -(2+$i) else $i end) as $i |
  "\($i+1):\($pos-.[$i]+1)";

def prog_with_eof:
  if .i != null and .i < (.src|length) then .prog
  else .prog + [{pos:.src|length, pc:.prog|length}] end;

def disasm:
  [.prog[] | inst_asm + "\n"] | join("");
def disasm_pc_insts($pc; $breaks):
  (last.pc|tostring|length) as $w |
  map(inst_asm_pc($pc; $breaks; $w)) | join("");
def disasm_pc:
  . as $state |
  prog_with_eof | disasm_pc_insts($state.pc; $state.breaks);
def trace($pc; $n_before; $n_after):
  .breaks as $breaks |
  prog_with_eof |
  if $pc < $n_before then .[:$pc+$n_after+1]
  else .[$pc-$n_before:$pc+$n_after+1] end |
  disasm_pc_insts($pc; $breaks);
def trace($pc; $n): trace($pc; $n; $n);

def dump_stack: .s | join(", ");
def dump_calls: [.c[] as $c | .prog[$c-1].arg] | join(", ");
def dump_heap_map:
  [(.h | keys_unsorted | sort_by(tonumber)[]) as $k | "\($k):\(.h[$k])"] |
  join(", ");
def dump_heap_table($cols):
  (.h | map(tostring | length) | max + 1) as $cell_width |
  (.h | keys_unsorted | map(length) | max + 1) as $addr_width |
  reduce (.h | to_entries[]) as $v ({};
    ($v.key|tonumber) as $key |
    ($key % $cols) as $col | (($key - $col) / $cols | tostring) as $row |
    .[$row] |= (. // []) |
    .[$row][$col] = $v.value) |
  reduce (keys_unsorted[] | tonumber) as $row (.;
    if .[$row+1|tostring] == null then
      .[$row+1|tostring] = if .[$row+2|tostring] != null then [] else null end
    else . end) |
  def pad_right($width): tostring | . + " " * ($width - length);
  def format_cells:
    if . == null or length == 0 then ""
    else
      map(. // "" | pad_right($cell_width)) |
      " " + join("") | gsub(" +$"; "")
    end;
  (" " * ($addr_width + 1)) as $empty_addr |
  $empty_addr + ([range($cols)] | format_cells) + "\n" +
  $empty_addr + "_" * ($cell_width * $cols + 1) + "\n" +
  (to_entries | sort_by(.key|tonumber)[:-1] |
    map(
      (if .value != null then .key|tonumber * $cols else "…" end) as $addr |
      ($addr | pad_right($addr_width)) +
      "|" + (.value | format_cells) + "\n") |
    join(""));
def dump_state:
  "Stack: [\(dump_stack)]\n" +
  "Calls: [\(dump_calls)]\n" +
  "Heap:  {\(dump_heap_map)}\n" +
  "\(dump_heap_table(10))";

def prefix_error: ("Error:"|bright_red) + " " + .;
def inst_error($msg; $inst; $pc):
  ($pc // .pc0 // .prog|length) as $pc |
  ($inst // .prog[$pc]) as $inst |
  ($msg|prefix_error)
  + if $inst.pos != null then
      " at \(inst_line($inst.pos)) (offset \($inst.pos))" else "" end
  + if $inst != null then ": \($inst | inst_str)" else "" end + "\n"
  + if .prog|length > 0 then "\n" + trace($pc; 4) else "" end
  + if .pc != null then "\n" + dump_state else "" end |
  halt_error(1);
def inst_error($msg; $inst): inst_error($msg; $inst; $inst.pc);
def inst_error($msg): inst_error($msg; null);

def assert($cond; msg; inst):
  if $cond then . else inst_error(msg; inst) end;
def assert($cond; msg): assert($cond; msg; null);

def match_char(s; t; l; eof):
  .src[.i] as $ch | .i+=1 |
  if   $ch == 32 then .tok += "[Space]" | s
  elif $ch == 9  then .tok += "[Tab]"   | t
  elif $ch == 10 then .tok += "[LF]"    | .lines += [.i] | l
  elif .i >= (.src|length) then eof
  else match_char(s; t; l; eof) end;
def match_char(s; t; l):
  match_char(s; t; l;
    inst_error("unexpected EOF"; {typ:(.tok+"[EOF]"), pos:(.i-1)}));

def parse_inst:
  def parse_num:
    match_char(
      .n*=2 | parse_num;         # 0 digit
      .n*=2 | .n+=1 | parse_num; # 1 digit
      .);                        # done
  def parse_lbl:
    def digit($d): .n*=2 | .n+=$d | .l+=[$d] | parse_lbl;
    match_char(digit(0); digit(1); .);
  def lbl_str:
    .n as $n | .l |
    if length == 0 then "%b" # empty binary string
    elif length%8 == 0 then
      [range(0;length;8) as $i |
        reduce .[$i:$i+8][] as $d (0; .*2 + $d)] |
      # Label is visible ASCII and doesn't start with %
      if all(33 <= . and . <= 126) and .[0] != 37
      then implode else . end
    else . end |
    if type != "array" then .
    elif length > 1 and .[0] == 0 then "%b\(join(""))"
    else "%\($n)" end;

  def inst($typ): .prog += [{typ:$typ, pos, pc:.prog|length}];
  def inst_num($typ):
    .n = 0 | match_char(parse_num; parse_num | .n*=-1; .) |
    if .n == 0 then .n = 0 else . end | # Normalize -0
    .prog += [{typ:$typ, arg:.n, pos, pc:.prog|length}] |
    del(.n);
  def inst_lbl($typ):
    .n = 0 | .l = [] | parse_lbl |
    .prog += [{typ:$typ, arg:lbl_str, pos, pc:.prog|length}] |
    del(.n, .l);
  def inst_err: inst_error("unrecognized instruction"; {typ:.tok, pos});

  .pos = .i | .tok = "" |
  match_char(
    # Stack
    match_char(
      inst_num("push");     # SS  n push
      match_char(
        inst_num("copy");   # STS n copy
        inst_err;
        inst_num("slide")); # STL n slide
      match_char(
        inst("dup");        # SLS   dup
        inst("swap");       # SLT   swap
        inst("drop")));     # SLL   drop
    match_char(
      # Arithmetic
      match_char(
        match_char(
          inst("add");      # TSSS  add
          inst("sub");      # TSST  sub
          inst("mul"));     # TSSL  mul
        match_char(
          inst("div");      # TSTS  div
          inst("mod");      # TSTT  mod
          inst_err);
        inst_err);
      # Heap
      match_char(
        inst("store");      # TTS   store
        inst("retrieve");   # TTT   retrieve
        inst_err);
      # I/O
      match_char(
        match_char(
          inst("printc");   # TLSS  printc
          inst("printi");   # TLST  printi
          inst_err);
        match_char(
          inst("readc");    # TLTS  readc
          inst("readi");    # TLTT  readi
          inst_err);
        inst_err));
    # Control flow
    match_char(
      match_char(
        inst_lbl("label");  # LSS l label
        inst_lbl("call");   # LST l call
        inst_lbl("jmp"));   # LSL l jmp
      match_char(
        inst_lbl("jz");     # LTS l jz
        inst_lbl("jn");     # LTT l jn
        inst("ret"));       # LTL   ret
      match_char(
        inst_err;
        inst_err;
        inst("end"));       # LLL   end
      .); # allow trailing LF
    .);

def label_map:
  . as $state |
  reduce (.prog[] | select(.typ == "label")) as $inst
    ({}; . as $labels |($inst.arg|tostring) as $lbl |
      $state | assert($labels[$lbl] == null; "label redefined"; $inst) |
      $labels | .[$lbl] = $inst.pc);

def parse:
  {
    src: explode, # program source
    i: 0,         # read offset
    pos: 0,       # instruction start offset
    tok: "",      # current token
    lines: [0],   # offsets of lines
    prog: [],     # instructions
  } |
  def _parse:
    if .i < (.src|length) then parse_inst | _parse else . end;
  _parse |
  del(.i, .pos, .tok) |
  .labels = label_map;

def floor_div($x; $y):
  ($x % $y) as $r |
  (($x - $r) / $y) as $q |
  if ($r > 0 and $y < 0) or ($r < 0 and $y > 0)
  then $q - 1 else $q end;
def floor_mod($x; $y):
  ($x % $y) as $r |
  if ($r > 0 and $y < 0) or ($r < 0 and $y > 0)
  then $r + $y else $r end;

def interpret_step:
  def assert_len($n): assert(.s|length >= $n; "stack underflow");
  def assert_ret: assert(.c|length >= 1; "call stack underflow");
  def push($n): .s += [$n];
  def pop: assert_len(1) | .s |= .[:-1];
  def at($n): assert_len($n+1) | .s[-$n-1];
  def top: at(0);
  def top2: at(1);
  def assert_div: assert(top != 0; "zero divisor");
  def store($addr; $val):
    .max_addr = ([.max_addr, $addr] | max) |
    .h[$addr|tostring] = $val;
  def retrieve($addr):
    assert((.check_retrieve|not) or $addr <= .max_addr;
      "retrieve above maximum stored address (\($addr) > \(.max_addr))") |
    .h[$addr|tostring] // 0;
  def jmp($l):
    assert(.labels|has($l); "undefined label") | .pc = .labels[$l];

  def print($op; format):
    format as $v |
    if .debug then
      ($op+">"|bright_cyan + " \($v | tojson)\n"),
        (.out += ($v | tostring) | pop)
    else ($v | tostring), pop end;
  def printc:
    top as $c |
    assert($c >= 0 and $c <= 1114111; "printing invalid codepoint (\($c))") |
    [$c] | implode;
  def read(handle_read):
    assert_len(1) |
    if .in_buf != "" or .eof then .
    else
      if .debug then "read<"|bright_cyan + " " else empty end,
      (. as $state |
        try (.in_buf = input + "\n")
        catch if . == "break" then $state | .eof = true else error end)
    end |
    if type != "object" then .
    else
      if .in_buf != "" then handle_read
      elif .on_eof|type == "number" then
        store(top; .on_eof) | pop | .in_consumed += ("[EOF]"|red)
      else inst_error("EOF") end
    end;
  def readc:
    store(top; (.in_buf|explode)[0]) | pop |
    .in_consumed += .in_buf[:1] | .in_buf |= .[1:];
  def readi:
    (.in_buf|index("\n")) as $i | .in_buf[:$i] as $line |
    .in_consumed += .in_buf[:$i+1] | .in_buf |= .[$i+1:] |
    (try ($line|tonumber) catch .5) as $n |
    assert(($n|. == trunc) and ($line | test("^\\s*[+-]?\\d+\\s*$"));
      "invalid integer " + ($line | tojson)) |
    store(top; $n) | pop;

  assert(.pc < (.prog|length); "interpreter stopped") |
  .prog[.pc] as $inst | $inst as {typ:$t, arg:$n} |
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
  elif $t == "div"      then assert_div | top2 = floor_div(top2; top) | pop
  elif $t == "mod"      then assert_div | top2 = floor_mod(top2; top) | pop
  elif $t == "store"    then store(top2; top) | pop | pop
  elif $t == "retrieve" then top = retrieve(top)
  elif $t == "label"    then .
  elif $t == "call"     then .c += [.pc] | jmp($n)
  elif $t == "jmp"      then jmp($n)
  elif $t == "jz"       then if top == 0 then jmp($n) else . end | pop
  elif $t == "jn"       then if top < 0 then jmp($n) else . end | pop
  elif $t == "ret"      then assert_ret | .pc = .c[-1] | .c |= .[:-1]
  elif $t == "end"      then .pc = (.prog|length)
  elif $t == "printc"   then print("printc"; printc)
  elif $t == "printi"   then print("printi"; top)
  elif $t == "readc"    then read(readc)
  elif $t == "readi"    then read(readi)
  else inst_error("malformed instruction") end;

def interpret_step_debug:
  def print_exit_status:
    if type != "object" or .pc < (.prog|length) then empty
    else
      if .prog[.pc0].typ == "end"
      then "[program exited cleanly]\n"|green
      else "[program exited implicitly]\n"|red end
    end;
  if .pc >= (.prog|length) then ("[interpreter stopped]\n"|red), .
  else
    .moved = true |
    interpret_step |
    print_exit_status, .
  end;

def interpret_init:
  . + {
    pc: 0,             # program counter
    pc0: 0,            # previous program counter
    s: [],             # data stack
    c: [],             # call stack
    h: {},             # heap
    in_buf: ($in//""), # stdin buffer
    in_consumed: "",   # input read from stdin
    out: "",           # output written to stdout
    max_addr: -1,      # maximum stored address
  };

def interpret_continue:
  # If parameters are recursively passed to interpret_contine, jq does
  # not detect the tail call and perform tail-call optimization.
  if type != "object" or .pc >= (.prog|length) then .
  else interpret_step | interpret_continue end;
def interpret_continue_debug:
  if type != "object" or .pc >= (.prog|length) then .
  elif .moved and .breaks[.pc|tostring] then
    ("[stopped at breakpoint]\n"|red), .
  else interpret_step_debug | interpret_continue_debug end;

def interpret_next:
  def _next($depth):
    if .debug then interpret_step_debug else interpret_step end |
    if type != "object" or .pc >= (.prog|length) then .
    elif .moved and .breaks[.pc|tostring] then
      ("[stopped at breakpoint]\n"|red), .
    else
      .prog[.pc0].typ as $typ |
      (if $typ == "call" then $depth+1
        elif $typ == "ret" then $depth-1
        else $depth end) as $depth |
      if $depth > 0 then _next($depth) else . end
    end;
  _next(0);

def check_clean_exit:
  if type == "object" and .check_clean then
    if .prog[.pc0].typ != "end" and (.s|length != 0) then
      inst_error("exited implicitly with non-empty stack")
    elif .prog[.pc0].typ != "end" then
      inst_error("exited implicitly")
    elif .s|length != 0 then
      inst_error("exited with non-empty stack")
    else . end
  else . end;
def interpret: interpret_init | interpret_continue | check_clean_exit;

def debug:
  def help:
    "Debugger commands:\n" +
    "  r, run        -- Run or restart the program from the start\n" +
    "  c, continue   -- Continue from the current instruction\n" +
    "  s, step       -- Execute next instruction, stepping into calls\n" +
    "  n, next       -- Execute next instruction, stepping over calls\n" +
    "  b, breakpoint -- Set or clear a breakpoint\n" +
    "  d, disasm     -- Disassemble program\n" +
    "  l, labels     -- List labels in program\n" +
    "  p, print      -- Dump the data stack, call stack, and heap\n" +
    "  i, input      -- Dump the input read from stdin so far\n" +
    "  o, output     -- Dump the output written to stdout so far\n" +
    "  q, quit       -- Quit the debugger\n" +
    "  h, help       -- Show a list of all debugger commands\n";

  def run:
    if .pc > 0 then ("[interpreter restarted]\n"|green), interpret_init
    else interpret_init | interpret_continue_debug end;
  def breakpoint($args):
    if $args|length > 1 then ("too many arguments\n"|prefix_error), .
    elif $args|length == 1 then
      def toggle: if . == null then true else not end;
      $args[0] as $v |
      if .labels|has($v) then
        .breaks[.labels[$v]|tostring] |= toggle
      else
        (try ($v|tonumber) catch -1) as $n |
        if 0 <= $n and $n < (.prog|length) and ($n|. == trunc) then
          .breaks[$v] |= toggle
        else ("label or pc not found: \($v)\n"|prefix_error), . end
      end
    else . end |
    if type == "object" then
      .breaks as $breaks |
      (.breaks | keys_unsorted | map(tonumber) | sort[]) as $b |
      .prog[$b] | inst_asm_pc(-1; $breaks; 0)
    else empty end, .;
  def list_labels:
    . as $state |
    [.prog[.labels | to_entries | sort_by(.value)[].value]] |
    disasm_pc_insts($state.pc; $state.breaks);
  def print_io:
    if .[-1:] != "\n" then . + ("⏎\n"|bright_black) else . end;

  def iscmd($cmd): . == $cmd or . == $cmd[:1];
  def _debug:
    if .moved and .pc < (.prog|length) then trace(.pc; 0; 3)
    else empty end,
    ("(wsjq)"|bright_black+" "),
    ((try input
      catch if . == "break" then "q" else error end) as $cmd |
    ($cmd | gsub("^\\s+|\\s+$"; "")) as $cmd |
    (if $cmd == "" then .cmd else $cmd end) as $cmd |
    ($cmd | split("\\s+"; "")) as $words |
    ($words[0] // "") as $c | $words[1:] as $args |
    .moved = false |
    if   $c == ""               then .
    elif $c|iscmd("run")        then .cmd = "" | run
    elif $c|iscmd("continue")   then .cmd = $cmd | interpret_continue_debug
    elif $c|iscmd("step")       then .cmd = $cmd | interpret_step_debug
    elif $c|iscmd("next")       then .cmd = $cmd | interpret_next
    elif $c|iscmd("breakpoint") then breakpoint($args)
    elif $c|iscmd("disasm")     then disasm_pc, .
    elif $c|iscmd("labels")     then list_labels, .
    elif $c|iscmd("print")      then dump_state, .
    elif $c|iscmd("input")      then (.in_consumed | print_io), .
    elif $c|iscmd("output")     then (.out | print_io), .
    elif $c|iscmd("quit")       then ""
    elif $c|iscmd("help")       then help, .
    else ("\($cmd|tojson) is not a valid command\n"|prefix_error), . end |
    if type != "object" then .
    else _debug end);

  . + {
    debug: true, # debugger mode
    cmd: "",     # previous debug command
    breaks: {},  # breakpoints (key: pc, value: enabled boolean)
    moved: true, # whether the last command moved pc
  } |
  interpret_init | _debug;
