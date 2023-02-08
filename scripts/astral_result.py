# Class wrapping Astral's result
#
# Author: Tomas Dacik (idacik@fit.vut.cz), 2022

import json

from dataclasses import dataclass


@dataclass
class Result:

    name: str
    status: str

    size: int

    location_bound: int

    @staticmethod
    def load(path):
        with open(path, "r") as f:
            result = json.load(f)

        name = result["Name"]
        status = result["Status"]
        size = int(result["Formula size"])
        location_bound = int(result["Heap location bound"])

        return Result(name, status, size, location_bound)
