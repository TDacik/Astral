# Runner of a single test case
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2024

import os
import yaml

from dataclasses import dataclass
from enum import Enum
from subprocess import run, PIPE, TimeoutExpired

from utils import *

ASTRAL = "_build/default/astral/main.exe"
TMP = "/tmp/astral"


@dataclass
class Config:
    ignore: bool = False
    verify_model: bool = False

    timeout: int = 10
    backend: str = "z3"
    encoding: str = "bitvectors"
    qf_encoding: str = "direct"
    separation: str = "weak"

    @classmethod
    def from_file(cls, path):
        with open(path, "r") as f:
            json = yaml.safe_load(f)
            if json is None:
                return Config()

        kwargs = {
            key.replace("-", "_"): value for key, value in json.items()
        }  # if key in vars(Config)}
        return Config(**kwargs)

    def to_command(self, path):
        name = os.path.basename(path)
        # fmt: off
        return ([
            ASTRAL,
            "--backend", self.backend,
            "--encoding", self.encoding,
            "--qf-encoding", self.qf_encoding,
            "--separation", self.separation,
            "--json-output", TMP + name + ".json",
        ]
        + (["--verify-model", "--produce-models"] if self.verify_model else [])
        + [path])
        # fmt: on


class Status(Enum):
    CORRECT = "OK"
    INCORRECT = "ERR"
    WRONG_MODEL = "WRONG MODEL"
    UNKNOWN = "UKNOWN"
    TIMEOUT = "TIMEOUT"
    MEMOUT = "OUT-OF-MEMORY"
    ERROR = "FAIL"

@dataclass
class Result:
    name: str
    status: Status
    return_code: int = 0
    msg: str = ""

    @classmethod
    def of_run(cls, name, process):
        stdout = process.stdout.decode().strip()
        stderr = process.stderr.decode().strip()

        #print(process.returncode)
        #print(stdout)
        #print(stderr)
        msg = "Model verified" if "Model verified" in stdout else ""

        if process.returncode == 0 and stdout.startswith("unknown"):
            status = Status.UNKNOWN
        elif process.returncode == 0:
            status = Status.CORRECT
        elif process.returncode == 1 and stderr.startswith("Killed"):
            status = Status.MEMOU
        elif "exception" in stderr:
            status = Status.ERROR
            msg = stderr
        elif process.returncode == 1:
            status = Status.INCORRECT
            msg = "status is not correct"
        elif process.returncode == 3:
            status = Status.UNKNOWN
            msg = "status is incorrect, but model was not verified"
        elif process.returncode == 4:
            status = Status.WRONG_MODEL
            msg = "status is correct, but model is wrong"
        elif (process.returncode == 2) or (process.returncode > 4):
            status = Status.ERROR
            msg = stderr
        else:
            assert False

        return Result(name, status, return_code=process.returncode, msg=msg)

    @classmethod
    def timeout(cls, name, to):
        return Result(name, Status.TIMEOUT)

    @property
    def is_correct(self):
        return self.status == Status.CORRECT

    @property
    def is_unknown(self):
        return self.status in [Status.UNKNOWN, Status.TIMEOUT, Status.MEMOUT]

    def is_error(self):
        return not self.is_correct and not self.is_unknown

    @property
    def message(self):
        if self.msg == "":
            return ""
        else:
            return " (" + self.msg + ")"

    def print(self):
        if self.status == Status.CORRECT:
            print_ok(f"[OK] {self.name}{self.message}")
        elif self.status == Status.UNKNOWN:
            print_unknown(f"[UNKNOWN] {self.name}{self.message}")
        elif self.status == Status.TIMEOUT:
            print_unknown(f"[TIMEOUT]: {self.name}{self.message}")
        elif self.status == Status.MEMOUT:
            print_unknown(f"[MEMOUT]: {self.name}{self.message}")
        elif self.status == Status.INCORRECT:
            print_err(f"[INCORRECT]: {self.name}{self.message}")
        elif self.status == Status.WRONG_MODEL:
            print_err(f"[WRONG_MODEL]: {self.name}{self.message}")
        else:
            print_err(f"[ERR {self.return_code}]: {self.name}{self.message}")


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
