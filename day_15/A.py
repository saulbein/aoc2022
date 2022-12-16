import re
from argparse import ArgumentParser
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Tuple

SENSOR_REGEX = (
    r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
)


@dataclass
class Point:
    x: int
    y: int


@dataclass(order=True)
class Range:
    left: int
    right: int

    @property
    def length(self) -> int:
        return self.right - self.left

    def is_touching(self, other: "Range") -> bool:
        return (
            (other.left <= self.right <= other.right)
            or (other.left <= self.right <= other.right)
            or (self.left <= other.left <= self.right)
            or (self.left <= other.right <= self.right)
            or (self.left + 1 == other.right)
            or (self.right - 1 == other.left)
            or (other.right + 1 == self.left)
            or (other.left - 1 == self.right)
        )

    def join(self, other: "Range") -> "Range":
        if not self.is_touching(other):
            raise ValueError(f"Tried to join non-touching ranges: {self}, {other}")
        return Range(
            left=min(self.left, other.left), right=max(self.right, other.right)
        )


@dataclass
class Sensor:
    location: Point
    closest_beacon: Point

    @property
    def reach(self) -> int:
        return manhattan(self.location, self.closest_beacon)


def manhattan(p1: Point, p2: Point) -> int:
    return abs(p1.x - p2.x) + abs(p1.y - p2.y)


def main() -> None:
    parser = ArgumentParser()
    parser.add_argument("-i", "--input", type=Path, required=True)
    parser.add_argument("-y", type=int, required=True)
    args = parser.parse_args()
    solve(args.input, args.y)


def solve(input_path: Path, y: int) -> None:
    with open(input_path, "r") as lines:
        sensors = (parse_sensor(line) for line in lines)
        coverages = (find_intersection(sensor, y) for sensor in sensors)
        relevant = (r for r in coverages if r is not None)
        ranges = simplify_ranges(relevant)
        print(sum((range.length for range in ranges)))


def parse_sensor(line: str) -> Sensor:
    match = re.match(SENSOR_REGEX, line)
    if not match:
        raise ValueError(f"Input '{line}' does not match regex '{SENSOR_REGEX}")
    sx, sy, bx, by = tuple(int(g) for g in match.groups())
    return Sensor(location=Point(x=sx, y=sy), closest_beacon=Point(x=bx, y=by))


def find_intersection(sensor: Sensor, y: int) -> Optional[Range]:
    if sensor.location.y <= y:
        max_y_reach = sensor.location.y + sensor.reach
        if not max_y_reach >= y:
            return None
        extra_reach = max_y_reach - y
    else:
        min_y_reach = sensor.location.y - sensor.reach
        if not min_y_reach <= y:
            return None
        extra_reach = y - min_y_reach
    return Range(
        left=sensor.location.x - extra_reach, right=sensor.location.x + extra_reach
    )


def simplify_ranges(ranges: Iterable[Range]) -> List[Range]:
    in_order: List[Range] = sorted(ranges)
    new_ranges: List[Range] = []
    if not in_order:
        return []

    r_acc = in_order[0]
    for r in in_order[1:]:
        if r.is_touching(r_acc):
            r_acc = r_acc.join(r)
        else:
            new_ranges.append(r_acc)
            r_acc = r
    if r_acc:
        new_ranges.append(r_acc)
    return new_ranges


if __name__ == "__main__":
    main()
