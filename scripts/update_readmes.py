"""Update README files with problem counts and solution tables."""
import re
from pathlib import Path
from urllib.parse import quote

TABLE_SEPARATOR = "|---|---|---|"
SRC_DIR = Path("./src")
README_EN = "README.md"
README_PT = "README-PT.md"
LINK_TEXT_EN = "View Solution"
LINK_TEXT_PT = "Acessar Solução"


def list_directories(path: Path) -> list[str]:
    """Return first-level directory names under path."""
    return [entry.name for entry in path.iterdir() if entry.is_dir()]


def parse_int(value: str) -> int:
    """Parse an integer from a table cell; returns 0 on failure."""
    digits = re.sub(r"[^0-9]", "", value)
    return int(digits) if digits else 0


def format_percentage(documented: int, total: int) -> str:
    if total <= 0:
        return "0%"
    return f"{round((documented / total) * 100)}%"


def split_table_row(line: str) -> list[str]:
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def update_header_problem_count(lines: list[str], new_count: int) -> list[str]:
    """Update the count in the header line matching '# Title (N/M)'."""
    pattern = re.compile(r"^(#\s*.*?\()(\d+)(/\d+\))\s*$")
    updated = False
    result: list[str] = []

    for line in lines:
        match = pattern.match(line)
        if match:
            line = f"{match.group(1)}{new_count}{match.group(3)}"
            updated = True
        result.append(line)

    if not updated:
        raise ValueError("Header line not found for count update.")
    return result


def replace_table_body(lines: list[str], table_body: str, *, truncate: bool = True) -> list[str]:
    """Replace table body after the separator row."""
    for index, line in enumerate(lines):
        if line == TABLE_SEPARATOR:
            return lines[: index + 1] + [table_body] if truncate else lines[: index + 1] + [table_body] + lines[index + 1 :]
    return lines


def update_category_readme(readme_path: Path, problem_count: int, table_body: str) -> None:
    """Update category README with new header count and solution table."""
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    lines = update_header_problem_count(lines, problem_count)
    lines = replace_table_body(lines, table_body, truncate=True)
    readme_path.write_text("\n".join(lines), encoding="utf-8")


def build_solution_table(problem_dirs: list[str], link_text: str, readme_name: str) -> str:
    """Build table rows from problem directory names (e.g. '123 - Problem Name')."""
    rows: list[str] = []
    for problem in problem_dirs:
        parts = problem.split(" - ", 1)
        problem_id = parts[0]
        problem_name = parts[1] if len(parts) > 1 else ""
        link = f"[{link_text}](./{quote(problem)}/{readme_name})"
        rows.append(f"| {problem_id} | {problem_name} | {link} |")
    return "\n".join(rows)


def is_category_row(line: str, category: str) -> bool:
    if "](./" not in line or "/README" not in line:
        return False
    return f"](./{quote(category)}/" in line


def is_total_row(line: str) -> bool:
    return line.strip().startswith("| Total |")


def compute_totals(lines: list[str]) -> tuple[int, int]:
    total_documented = 0
    total_all = 0
    for line in lines:
        if "](./" in line and "/README" in line and not is_total_row(line):
            cells = split_table_row(line)
            if len(cells) >= 4:
                total_documented += parse_int(cells[1])
                total_all += parse_int(cells[2])
    return total_documented, total_all


def update_platform_readme(readme_path: Path, category: str, documented_count: int) -> None:
    """Update category row and recompute Total row in the platform README."""
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    updated_lines: list[str] = []

    for line in lines:
        if line.strip().startswith("|") and is_category_row(line, category):
            cells = split_table_row(line)
            if len(cells) >= 4:
                total_count = parse_int(cells[2])
                percentage = format_percentage(documented_count, total_count)
                line = f"| {cells[0]} | {documented_count} | {total_count} | {percentage} |"
        updated_lines.append(line)

    total_documented, total_all = compute_totals(updated_lines)
    total_percentage = format_percentage(total_documented, total_all)

    final_lines: list[str] = []
    for line in updated_lines:
        if is_total_row(line):
            line = f"| Total | {total_documented} | {total_all} | {total_percentage} |"
        final_lines.append(line)

    readme_path.write_text("\n".join(final_lines), encoding="utf-8")


def main() -> None:
    for platform in list_directories(SRC_DIR):
        platform_path = SRC_DIR / platform

        for category in list_directories(platform_path):
            category_path = platform_path / category
            problem_dirs = list_directories(category_path)

            if not problem_dirs:
                continue

            problem_count = len(problem_dirs)
            table_en = build_solution_table(problem_dirs, LINK_TEXT_EN, README_EN)
            table_pt = build_solution_table(problem_dirs, LINK_TEXT_PT, README_PT)

            update_category_readme(category_path / README_EN, problem_count, table_en)
            update_category_readme(category_path / README_PT, problem_count, table_pt)

            # add here
            update_platform_readme(platform_path / README_EN, category, problem_count)
            update_platform_readme(platform_path / README_PT, category, problem_count)


if __name__ == "__main__":
    main()