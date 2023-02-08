# Automatic checker of additional properties of benchmarks
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2022

from utils import *
from sl_parser import parse
from astral_result import Result


def check_location_bound_min(result, expected):
    actual = result.location_bound
    if actual < expected:
        print_err(
            f" -> location bound {actual} is lesser than expected lower bound {expected}",
            level=1,
        )


def check_location_bound_max(result, expected):
    actual = result.location_bound
    if actual > expected:
        print_err(
            f" -> location bound {actual} is greater than expected upper bound {expected}",
            level=1,
        )


def check_property(result, args):
    attr = args[0]
    value = args[1]

    if attr == ":location_bound_min":
        check_location_bound_min(result, int(value))
    if attr == ":location_bound_max":
        check_location_bound_max(result, int(value))


def check(benchmark, solver_result):
    script = parse(benchmark)
    result = Result.load(solver_result)
    for cmd in script:
        if cmd.name == "set-info":
            check_property(result, cmd.args)
