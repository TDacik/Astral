# Basic parsing of meta data from separation logic format
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2022


class Command:
    def __init__(self, name, args):
        self.name = name
        self.args = args


def parse(benchmark):
    with open(benchmark, "r") as f:
        script = f.read()

    acc = []
    for line in script.split("\n"):
        if line.startswith("(set-info"):
            acc.append(Command("set-info", line[1:-1].split(" ")[1:]))
    return acc
