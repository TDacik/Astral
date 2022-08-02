import os
from subprocess import run, PIPE, TimeoutExpired

class colors:
    red = "\033[91m"
    green = "\033[92m"
    white = "\033[m"

def print_ok(text):
    print(f"{colors.green}{text}{colors.white}")

def print_err(text):
    print(f"{colors.red}{text}{colors.white}")

def print_bench_name(root, dirs):
    print(os.path.basename(root))

class Runner:
    def __init__(self, backend="z3", timeout=10):
        self.backend = backend
        self.timeout = timeout

        self.correct = 0
        self.incorrect = 0
        self.unknown = 0

        self.errors = 0
        self.timeouts = 0

    def smoke_test(self):
        """Verify whether Astral is correctly installed."""
        try:
            process = run(["astral", "--help"], stdout=PIPE, stderr=PIPE)
        except FileNotFoundError:
            print("Astral is not correctly installed")
            exit(1)

    def run(self, path, name):
        command = ["astral", "--backend", self.backend, os.path.join(path,name)]
        try:
            process = run(command, timeout=self.timeout, stdout=PIPE, stderr=PIPE)
            stdout = process.stdout.decode().strip()
            stderr = process.stderr.decode().strip()

            if process.returncode == 0:
                print_ok(f"[OK] {name}: {stdout}")
                self.correct += 1

            elif process.returncode == 2:
                print_err(f"[INCORRECT]: {name}: {stdout}")
                self.incorrect += 1

            else:
                print_err(f"[ERR {process.returncode}]: {name}: {stdout}")
                self.errors += 1

        except TimeoutExpired as to:
            print_err(f"[TO]: {name}")
            self.timeouts +=1


if __name__ == "__main__":
    runner = Runner()
    runner.smoke_test()

    for root, dirs, files in sorted(os.walk("../benchmarks/")):
        # Ignore debug directories
        if os.path.basename(root) == "astral_debug":
            continue

        print_bench_name(root, dirs)
        for f in sorted(files):
            if f.endswith(".smt2"):
                runner.run(root, f)

    if runner.incorrect > 0 or runner.errors > 0 or runner.timeouts > 0:
        exit(1)
