#!/usr/bin/env python3

import sys

tape = []
mem  = [ 0 ]

tape_index = -1
mem_index  = 0

getch = lambda: sys.stdin.read(1)
# try:
#     import msvcrt
#     getch = msvcrt.getch
# except:
#     import sys, tty, termios
#     def _unix_getch():
#         """Get a single character from stdin, Unix version"""
# 
#         fd = sys.stdin.fileno()
#         old_settings = termios.tcgetattr(fd)
#         try:
#             tty.setraw(sys.stdin.fileno())          # Raw read
#             ch = sys.stdin.read(1)
#         finally:
#             termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
#         return ch
# 
#     getch = _unix_getch

def next() -> str:
    global tape_index
    tape_index += 1
    if tape_index >= len(tape):
        tape.append(getch())

    return tape[tape_index]

def seek(dir: bool) -> str:
    global tape_index

    step  = 1 if dir else -1
    sym = "]" if dir else "["
    asym = "[" if dir else "]"

    asym_count = 0
    i = tape_index + step
    while True:
        if i >= len(tape):
            tape.append(getch())
        if i < 0:
            break

        if tape[i] == sym:
            if asym_count == 0:
                tape_index = i
                return
            asym_count -= 1
        if tape[i] == asym:
            asym_count += 1

        i += step

    print("Error: Unmatched %s" % sym)
    sys.exit(1)

def main():
    global mem_index
    while True:
        c = next()

        if c == "[":
            if mem[mem_index] == 0:
                seek(True)
        elif c == "]":
            if mem[mem_index] != 0:
                seek(False)
        elif c == "+":
            mem[mem_index] += 1
        elif c == "-":
            mem[mem_index] -= 1
        elif c == ".":
            print(chr(mem[mem_index]), end="", flush=True)
        elif c == ",":
            mem[mem_index] = ord(getch())
        elif c == ">":
            mem_index += 1
            if mem_index >= len(mem):
                mem.append(0)
        elif c == "<":
            mem_index -= 1
            if mem_index < 0:
                mem.prepend(0)
        elif c == "":
            break

if __name__ == "__main__":
    main()
