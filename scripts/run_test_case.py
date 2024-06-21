# Runner of a single test case
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2024

import os
import yaml

from dataclasses import dataclass
from enum import Enum
from subprocess import run, PIPE, TimeoutExpired

from utils import *

ASTRAL = "_build/default/main.exe"
TMP = "/tmp/astral"


@dataclass
class Config:
    ignore: bool = False
    verify_model: bool = False  # TODO: implement

    timeout: int = 10
    backend: str = "z3"
    encoding: str = "bitvectors"
    qf_encoding: str = "direct"
    separation: str = "weak"

    @classmethod
    def from_file(cls, path):
        with open(path, "r") as f:
            json = yaml.safe_load(f)

        kwargs = {
            key.replace("-", "_"): value for key, value in json.items()
        }  # if key in vars(Config)}
        return Config(**kwargs)

    def to_command(self, path):
        name = os.path.basename(path)
        # fmt: off
        return [
            ASTRAL,
            "--backend", self.backend,
            "--encoding", self.encoding,
            "--qf-encoding", self.qf_encoding,
            "--separation", self.separation,
            "--json-output", TMP + name + ".json",
            path
        ]
        # fmt: on


class Status(Enum):
    CORRECT = 0
    INCORRECT = 1
    UNKNOWN = 2
    TIMEOUT = 3
    ERROR = 4


@dataclass
class Result:
    name: str
    status: Status
    return_code: int = 0

    @classmethod
    def of_run(cls, name, process):
        stdout = process.stdout.decode().strip()
        stderr = process.stderr.decode().strip()

        if process.returncode == 0 and stdout.startswith("unknown"):
            status = Status.UNKNOWN
        elif process.returncode == 0:
            status = Status.CORRECT
        elif process.returncode == 1:
            status = Status.INCORRECT
        else:
            status = Status.ERROR

        return Result(name, status, process.returncode)

    @classmethod
    def timeout(cls, name, to):
        return Result(name, Status.TIMEOUT)

    @property
    def is_correct(self):
        return self.status == Status.CORRECT

    @property
    def is_unknown(self):
        return self.status in [Status.UNKNOWN, Status.TIMEOUT]

    def is_error(self):
        return not self.is_correct and not self.is_unknown

    def print(self):
        if self.status == Status.CORRECT:
            print_ok(f"[OK] {self.name}")
        elif self.status == Status.UNKNOWN:
            print_unknown(f"[UNKNOWN] {self.name}")
        elif self.status == Status.TIMEOUT:
            print_unknown(f"[TIMEOUT]: {self.name}")
        elif self.status == Status.INCORRECT:
            print_err(f"[INCORRECT]: {self.name}")
        else:
            print_err(f"[ERR {self.return_code}]: {self.name}")


class TestRunner:
    def __init__(self, path, config=Config()):
        self.path = path
        self.name = os.path.basename(path)
        self.config = config
        self.result = None

    def run(self):
        command = self.config.to_command(self.path)
        try:
            process = run(
                command, timeout=self.config.timeout, stdout=PIPE, stderr=PIPE
            )
            result = Result.of_run(self.name, process)
        except TimeoutExpired as to:
            result = Result.timeout(self.name, to)

        result.print()
        return result
