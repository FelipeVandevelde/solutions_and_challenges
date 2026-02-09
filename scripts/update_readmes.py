"""Update category README files with problem counts and solution tables."""

import os
import re
from pathlib import Path
from urllib.parse import quote

# Table format expected in READMEs
TABLE_SEPARATOR = "|---|---|---|"

# Source tree and README filenames
SRC_DIR = Path("./src")
README_EN = "README.md"
README_PT = "README-PT.md"
LINK_TEXT_EN = "View Solution"
LINK_TEXT_PT = "Acessar Solução"


def list_directories(path: str | Path) -> list[str]:
    """Return first-level directory names under path."""
    path = Path(path)
    with os.scandir(path) as entries:
        return [e.name for e in entries if e.is_dir()]


def update_header_count(lines: list[str], new_count: str) -> list[str]:
    """Update the count in the header line matching '# Title (N/M)'."""
    pattern = re.compile(r"^(#\s*.*?\()(\d+)(/\d+\))\s*$")
    result = []
    updated = False

    for line in lines:
        match = pattern.match(line)
        if match:
            line = f"{match.group(1)}{new_count}{match.group(3)}"
            updated = True
        result.append(line)

    if not updated:
        raise ValueError("Header line not found for count update.")
    return result


def rebuild_table(lines: list[str], table_body: str) -> list[str]:
    """Replace table body: keep content up to and including separator, then append table_body."""
    for i, line in enumerate(lines):
        if line == TABLE_SEPARATOR:
            return lines[: i + 1] + [table_body]
    return lines


def update_readme(readme_path: Path, new_count: str, table_body: str) -> None:
    """Update README with new header count and solution table."""
    path = Path(readme_path)
    lines = path.read_text(encoding="utf-8").splitlines()
    lines = update_header_count(lines, new_count)
    lines = rebuild_table(lines, table_body)
    path.write_text("\n".join(lines), encoding="utf-8")


def build_table_body(problems: list[str], link_text: str, readme_name: str) -> str:
    """Build table rows string from problem directory names (e.g. '123 - Problem Name')."""
    rows = []
    for problem in problems:
        parts = problem.split(" - ", 1)
        problem_id = parts[0]
        problem_name = parts[1] if len(parts) > 1 else ""
        link = f"[{link_text}](./{quote(problem)}/{readme_name})"
        rows.append(f"| {problem_id} | {problem_name} | {link} |")
    return "\n".join(rows)


def main() -> None:
    for platform in list_directories(SRC_DIR):
        platform_path = SRC_DIR / platform

        for category in list_directories(platform_path):
            category_path = platform_path / category
            problems = list_directories(category_path)

            if not problems:
                continue

            count = str(len(problems))
            table_en = build_table_body(problems, LINK_TEXT_EN, README_EN)
            table_pt = build_table_body(problems, LINK_TEXT_PT, README_PT)

            update_readme(category_path / README_EN, count, table_en)
            update_readme(category_path / README_PT, count, table_pt)


if __name__ == "__main__":
    main()
