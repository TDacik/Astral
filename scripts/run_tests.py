# Runner of regression tests
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2022

import os
import shutil

from subprocess import run, PIPE, TimeoutExpired

from utils import *
from property_checker import check

astral_bin = "astral"


def print_bench_name(root, dirs):
    print(os.path.basename(root))


class Runner:
    def __init__(
        self, backend="z3", encoding="bitvectors", qf_encoding="direct", timeout=10
    ):
        self.backend = backend
        self.encoding = encoding
        self.qf_encoding = qf_encoding

        self.timeout = timeout

        self.correct = 0
        self.incorrect = 0
        self.unknown = 0

        self.errors = 0
        self.timeouts = 0

    def smoke_test(self):
        """Verify whether Astral is correctly installed."""
        try:
            process = run([astral_bin, "--help"], stdout=PIPE, stderr=PIPE)
        except FileNotFoundError:
            print_err("Astral is not correctly installed")
            exit(1)

    def run(self, path, name):
        # fmt: off
        command = [
            astral_bin,
            "--backend", self.backend,
            "--encoding", self.encoding,
            "--qf-encoding", self.qf_encoding,
            "--separation", "weak",
            "--json-output", "/tmp/astral/" + name + ".json",
            os.path.join(path, name),
        ]
        # fmt: on
        try:
            process = run(command, timeout=self.timeout, stdout=PIPE, stderr=PIPE)
            stdout = process.stdout.decode().strip()
            stderr = process.stderr.decode().strip()

            if process.returncode == 0:
                print_ok(f"[OK] {name}: {stdout}")
                self.correct += 1

                # Check additional properties
                #check(os.path.join(path, name), "/tmp/astral/" + name + ".json")

            elif process.returncode == 2:
                print_err(f"[INCORRECT]: {name}: {stdout}")
                print(stdout)
                print(stderr)
                self.incorrect += 1

            else:
                print_err(f"[ERR {process.returncode}]: {name}: {stdout}")
                self.errors += 1

        except TimeoutExpired as to:
            print_unknown(f"[TO]: {name}")
            self.timeouts += 1

    def print_result(self):
        if runner.incorrect > 0:
            print_err(" - incorrect: ", runner.incorrect)
        if runner.errors > 0:
            print_err(" - errors: ", runner.incorrect)
        if runner.timeouts > 0:
            print_err(" - timeouts: ", runner.timeouts)

    def clean(self):
        shutil.rmtree("/tmp/astral", ignore_errors=True)

    def init(self):
        self.clean()
        os.mkdir("/tmp/astral")

    def run_one(self):
        self.init()
        for root, dirs, files in sorted(os.walk("benchmarks/")):
            if os.path.basename(root) in ["astral_debug"]:
                # Ignore debug directories
                continue

            print_bench_name(root, dirs)
            for f in sorted(files):
                if f.endswith(".smt2"):
                    self.run(root, f)
        self.clean()


if __name__ == "__main__":
    runner = Runner()
    runner.smoke_test()
    runner.run_one()
