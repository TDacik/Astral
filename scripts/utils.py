# Utilities
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2023


class colors:
    red = "\033[91m"
    green = "\033[92m"
    yellow = "\033[93m"
    white = "\033[m"


def print_indented(text, indent):
    indent = " " * indent
    print(f"{indent}{text}")


def print_ok(text, level=0):
    print_indented(f"{colors.green}{text}{colors.white}", level)


def print_err(text, level=0):
    print_indented(f"{colors.red}{text}{colors.white}", level)


def print_unknown(text, level=0):
    print_indented(f"{colors.yellow}{text}{colors.white}", level)
