# Runner of all test cases
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2024

import os
import shutil
import argparse

from utils import *
from subprocess import run, PIPE
from run_test_case import Config, TestRunner, Status

ASTRAL = "_build/default/astral/main.exe"
TMP = "/tmp/astral"


class Runner:
    def __init__(self, args, verbose=True):
        self.default_config = Config(backend=args.backend)
        self.verbose = verbose

        self.results = {}

    def print_bench_name(self, root, ignore=False):
        if self.verbose:
            if ignore:
                print(os.path.basename(root), "[Skipped]")
            else:
                print(os.path.basename(root))

    def smoke_test(self):
        """Verify whether Astral is correctly installed."""
        try:
            pass
            process = run([ASTRAL, "--help"], stdout=PIPE, stderr=PIPE)
        except FileNotFoundError:
            print_err("Astral is not correctly installed")
            exit(1)

    def init(self):
        shutil.rmtree(TMP, ignore_errors=True)
        os.mkdir(TMP)

    def run_test_case(self, path, config):
        runner = TestRunner(path, config)
        return runner.run()

    def run_test_suite(self, directory, files):
        # Load configuration, if specified
        try:
            config = Config.from_file(os.path.join(directory, "config.yaml"), self.default_config)
        except FileNotFoundError:
            config = self.default_config

        self.print_bench_name(directory, ignore=config.ignore)
        if config.ignore:
            return

        print(config)
        for f in sorted(files):
            if f.endswith(".smt2"):
                path = os.path.join(directory, f)
                result = self.run_test_case(path, config)
                self.results[result.path] = result

    def run_all(self, args):
        for root, dirs, files in sorted(os.walk("benchmarks/")):
            id_prefixes = ["21", "22", "23"]
            if args.ids and not any(root.startswith("benchmarks/" + p) for p in id_prefixes):
                continue

            if "astral_debug" in root or root == "benchmarks/":
                # Ignore debug
                continue

            self.run_test_suite(root, files)

    def report(self):
        correct = len([x for x in self.results.values() if x.status == Status.CORRECT])
        incorrect = len(
            [x for x in self.results.values() if x.status == Status.INCORRECT]
        )
        timeout = len([x for x in self.results.values() if x.status == Status.TIMEOUT])
        error = len([x for x in self.results.values() if x.status == Status.ERROR])
        unknown = len([x for x in self.results.values() if x.status == Status.UNKNOWN])

        total = len(self.results)

        print(f"\nTotal tests: {total}")

        if incorrect > 0:
            print_err(f" - incorrect: {incorrect}")
        if error > 0:
            print_err(f" - errors: {error}")
        if timeout > 0:
            print_unknown(f" - timeout: {timeout}")
        if unknown > 0:
            print_unknown(f" - unknown: {unknown}")

        if incorrect + error > 0:
            return 1
        return 0

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ids", action="store_true")
    parser.add_argument("--backend", default="bitwuzla") # TODO: what should be default?
    return parser.parse_args()

def main():
    args = parse_args()

    runner = Runner(args)
    runner.init()
    runner.smoke_test()
    runner.run_all(args)

    exit(runner.report())

if __name__ == "__main__":
    main()
