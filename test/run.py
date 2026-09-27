#!/usr/bin/env python3
"""Compile, link, and run the Cx language test cases."""

from __future__ import annotations

import argparse
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def read_summary(path: Path, cases_dir: Path) -> list[tuple[Path, int]]:
    tests: list[tuple[Path, int]] = []
    for line_number, raw_line in enumerate(path.read_text().splitlines(), 1):
        line = raw_line.split("#", 1)[0].strip()
        if not line:
            continue
        fields = line.split()
        if len(fields) != 2:
            raise ValueError(f"{path}:{line_number}: expected '<case> <exit-code>'")
        case_name, expected_text = fields
        try:
            expected = int(expected_text, 10)
        except ValueError as exc:
            raise ValueError(
                f"{path}:{line_number}: invalid exit code {expected_text!r}"
            ) from exc
        if not 0 <= expected <= 255:
            raise ValueError(
                f"{path}:{line_number}: exit code must be between 0 and 255"
            )
        case = (cases_dir / case_name).resolve()
        try:
            case.relative_to(cases_dir.resolve())
        except ValueError as exc:
            raise ValueError(
                f"{path}:{line_number}: case escapes cases directory"
            ) from exc
        if case.suffix != ".cx":
            raise ValueError(f"{path}:{line_number}: case must have a .cx suffix")
        if not case.is_file():
            raise ValueError(f"{path}:{line_number}: case does not exist: {case_name}")
        tests.append((case, expected))
    if not tests:
        raise ValueError(f"{path}: no test cases")
    return tests


def run_command(command: list[str], *, cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=cwd, text=True, capture_output=True)


def display_command(command: list[str]) -> str:
    return " ".join(subprocess.list2cmdline([part]) for part in command)


def run_test(compiler: Path, case: Path, expected: int, output_dir: Path) -> bool:
    name = case.stem
    object_file = output_dir / f"{name}.o"
    executable = output_dir / name
    compile_command = [str(compiler), "-o", str(object_file), str(case)]
    result = run_command(compile_command, cwd=output_dir)
    if result.returncode != 0:
        print(f"FAIL {case.name}: compile failed")
        print(f"  {display_command(compile_command)}")
        if result.stderr:
            print(result.stderr, end="")
        return False

    link_command = ["cc", "-no-pie", str(object_file), "-o", str(executable)]
    result = run_command(link_command, cwd=output_dir)
    if result.returncode != 0:
        print(f"FAIL {case.name}: link failed")
        print(f"  {display_command(link_command)}")
        if result.stderr:
            print(result.stderr, end="")
        return False

    result = subprocess.run(
        [str(executable)], cwd=output_dir, text=True, capture_output=True
    )
    if result.returncode != expected:
        print(f"FAIL {case.name}: expected exit {expected}, got {result.returncode}")
        if result.stdout:
            print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="")
        return False

    print(f"PASS {case.name} (exit {result.returncode})")
    return True


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("compiler", type=Path, help="path to the cxcc executable")
    parser.add_argument("--summary", type=Path, default=None)
    parser.add_argument(
        "--keep", action="store_true", help="keep per-test build artifacts"
    )
    args = parser.parse_args()

    root = Path(__file__).resolve().parent
    summary = (args.summary or root / "summary").resolve()
    cases_dir = root / "cases"
    compiler = args.compiler.resolve()
    if not compiler.is_file():
        print(f"error: compiler does not exist: {compiler}", file=sys.stderr)
        return 2
    if shutil.which("cc") is None:
        print("error: cannot find C compiler 'cc'", file=sys.stderr)
        return 2

    try:
        tests = read_summary(summary, cases_dir)
    except ValueError as error:
        print(f"error: {error}", file=sys.stderr)

        return 2

    temporary = (
        None if args.keep else tempfile.TemporaryDirectory(prefix="cxlang-tests-")
    )
    try:
        output_dir = Path(temporary.name) if temporary else root / ".build"
        output_dir.mkdir(parents=True, exist_ok=True)
        passed = sum(
            run_test(compiler, case, expected, output_dir) for case, expected in tests
        )
    finally:
        if temporary:
            temporary.cleanup()

    print(f"{passed}/{len(tests)} tests passed")
    return 0 if passed == len(tests) else 1


if __name__ == "__main__":
    raise SystemExit(main())
