"""
Auto-update README files under `./src`.

What this script does
- For each platform (e.g. `src/beecrowd/`) and each category under it (e.g. `beginner/`):
  - Updates the category README header with the number of problems found.
  - Regenerates the category problem table (ID, title, link to each problem README).
  - Updates each problem README by appending (or replacing) a "solutions" section containing
    the full code of each solution file in Markdown code blocks.
  - Updates the platform README by refreshing the category row and the "Total" row.

Important constraints
- The script relies on fixed markers in the README files:
  - A Markdown table separator row: `|---|---|---|`
  - A solutions marker: `<!-- solutions -->`
"""

import re
from pathlib import Path
from urllib.parse import quote

# ---------------------------------- Constants ---------------------------------

# Markdown markers used to locate where content must be replaced.
TABLE_SEPARATOR_ROW = "|---|---|---|"
SOLUTIONS_MARKER = "<!-- solutions -->"

# Repository layout assumptions.
SRC_DIR = Path("./src")
README_EN_FILENAME = "README.md"
README_PT_FILENAME = "README-PT.md"

# Link text shown inside generated tables.
SOLUTION_LINK_TEXT_EN = "View Solution"
SOLUTION_LINK_TEXT_PT = "Acessar Solução"

# Maps a solution file extension to a display label.
LANGUAGE_BY_EXTENSION = {
    ".c": "C99",
    ".cpp": "C++20",
    ".cs": "C#",
    ".java": "Java 19",
    ".js": "JavaScript 12.18",
    ".py": "Python 3.9",
    ".php": "PHP",
    ".dart": "Dart 3.0",
    ".kt": "Kotlin",
    ".rb": "Ruby 2.7",
    ".go": "Go 1.20",
    ".pp": "Pascal",
    ".rs": "Rust",
    ".lua": "Lua 5.4",
    ".ts": "TypeScript 5.1",
    ".r": "R",
    ".swift": "Swift 5.8",
    ".clj": "Clojure",
    ".hs": "Haskell",
    ".scala": "Scala",
    ".exs": "Elixir",
    ".ml": "OCaml",
    ".jl": "Julia",
}

# ------------------------------- Small file utils ------------------------------


def list_child_directories(
    parent_dir: Path
) -> list[str]:
    """
    Return the names of first-level directories inside `parent_dir`.

    The returned list is sorted for deterministic output.
    """
    return sorted(entry.name for entry in parent_dir.iterdir() if entry.is_dir())


def list_child_files(
    parent_dir: Path
) -> list[str]:
    """
    Return the names of first-level files inside `parent_dir`.

    The returned list is sorted for deterministic output.
    """
    return sorted(entry.name for entry in parent_dir.iterdir() if entry.is_file())


# ------------------------------ Markdown/table utils ----------------------------


def parse_int_from_cell(
    cell_value: str
) -> int:
    """
    Extract an integer from a Markdown table cell value.

    Examples
    - " 12 " -> 12
    - "12/34" -> 1234 (digits are concatenated)

    Returns 0 when no digits are found.
    """
    digits = re.sub(r"[^0-9]", "", cell_value)
    return int(digits) if digits else 0


def format_percentage(
    documented: int,
    total: int
) -> str:
    """Return a rounded percentage string like '75%'."""
    if total <= 0:
        return "0%"
    return f"{round((documented / total) * 100)}%"


def split_markdown_table_row(
    line: str
) -> list[str]:
    """
    Split a Markdown table row into trimmed cell values.

    Input:  "| A | B | C |"
    Output: ["A", "B", "C"]
    """
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def replace_section_after_exact_marker(
    lines: list[str],
    *,
    marker_line: str,
    new_body: str,
    truncate_after_marker: bool = True,
) -> list[str]:
    """
    Replace everything after a marker line with `new_body`.

    If `truncate_after_marker` is True (default), the file becomes:
      <content up to marker> + <marker> + <new_body>

    If False, only inserts `new_body` after marker keeping the rest.
    """
    for index, line in enumerate(lines):
        if line == marker_line:
            if truncate_after_marker:
                return lines[: index + 1] + [new_body]
            return lines[: index + 1] + [new_body] + lines[index + 1 :]
    return lines


def update_header_problem_count(
    lines: list[str],
    new_count: int
) -> list[str]:
    """
    Update the first Markdown H1 header that follows the pattern:
    '# <anything> (N/M)'

    Only `N` (documented count) is replaced with `new_count`.
    """
    header_pattern = re.compile(r"^(#\s*.*?\()(\d+)(/\d+\))\s*$")
    updated = False
    updated_lines: list[str] = []

    for line in lines:
        match = header_pattern.match(line)
        if match:
            line = f"{match.group(1)}{new_count}{match.group(3)}"
            updated = True
        updated_lines.append(line)

    if not updated:
        raise ValueError("Header line not found for count update.")
    return updated_lines


# ---------------------------- Markdown content builders -------------------------


def build_problem_solutions_markdown(
    problem_dir: Path,
    solution_filenames: list[str]
) -> str:
    """
    Build the Markdown section that embeds solution code files.

    Output format (repeated per solution):
    - A '### <Language>' title
    - A fenced code block with the file content

    Notes
    - The code fence language tag uses the file extension (e.g. 'py', 'cpp').
    - Unknown extensions are still embedded, but the title falls back to the extension.
    """
    rows: list[str] = []
    for filename in solution_filenames:
        extension = Path(filename).suffix
        language_label = LANGUAGE_BY_EXTENSION.get(extension, extension.lstrip(".").upper())
        code_fence_language = filename.rsplit(".", 1)[-1] if "." in filename else ""

        code = f"```{code_fence_language}\n"
        code += (problem_dir / "solutions" / filename).read_text(encoding="utf-8")
        code += "\n```"
        rows.append(f"### {language_label}\n{code}")

    return "\n".join(rows)


def build_category_problem_table_markdown(
    problem_dirnames: list[str],
    *,
    link_text: str,
    readme_filename: str,
) -> str:
    """
    Build Markdown table rows for a category README.

    Each `problem_dirname` is expected to look like: '123 - Problem Name'.
    """
    rows: list[str] = []

    for dirname in problem_dirnames:
        parts = dirname.split(" - ", 1)
        problem_id = parts[0]
        problem_name = parts[1] if len(parts) > 1 else ""
        link = f"[{link_text}](./{quote(dirname)}/{readme_filename})"
        rows.append(f"| {problem_id} | {problem_name} | {link} |")

    return "\n".join(rows)


# ------------------------------- README updaters --------------------------------


def update_category_readme(
    readme_path: Path,
    problem_count: int,
    table_body: str
) -> None:
    """
    Update a category README file.

    - Updates the header documented problem count.
    - Replaces the table body (after the separator row) with `table_body`.
    """
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    lines = update_header_problem_count(lines, problem_count)
    lines = replace_section_after_exact_marker(
        lines,
        marker_line=TABLE_SEPARATOR_ROW,
        new_body=table_body,
        truncate_after_marker=True,
    )
    readme_path.write_text("\n".join(lines), encoding="utf-8")


def update_problem_readme(
    readme_path: Path,
    solutions_body: str
) -> None:
    """
    Update a problem README file by replacing the solutions section.

    The solutions section is identified by an exact marker line: `<!-- solutions -->`.
    """
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    lines = replace_section_after_exact_marker(
        lines,
        marker_line=SOLUTIONS_MARKER,
        new_body=solutions_body,
        truncate_after_marker=True,
    )
    readme_path.write_text("\n".join(lines), encoding="utf-8")


def is_platform_category_row(
    line: str,
    category_dirname: str
) -> bool:
    """
    Check if a platform README table row corresponds to a given category.

    The heuristic looks for Markdown links to './<category>/README...'
    """
    if "](./" not in line or "/README" not in line:
        return False
    return f"](./{quote(category_dirname)}/" in line


def is_total_row(
    line: str
) -> bool:
    """Return True if the row is the platform README 'Total' row."""
    return line.strip().startswith("| Total |")


def compute_platform_totals(
    lines: list[str]
) -> tuple[int, int]:
    """
    Compute totals from a platform README table.

    Returns (documented_total, overall_total).
    """
    documented_total = 0
    overall_total = 0

    for line in lines:
        if "](./" in line and "/README" in line and not is_total_row(line):
            cells = split_markdown_table_row(line)
            if len(cells) >= 4:
                documented_total += parse_int_from_cell(cells[1])
                overall_total += parse_int_from_cell(cells[2])

    return documented_total, overall_total


def update_platform_readme(
    readme_path: Path,
    category_dirname: str,
    documented_count: int
) -> None:
    """
    Update a platform README file.

    - Updates the row for `category_dirname` with the new `documented_count`.
    - Recomputes and rewrites the 'Total' row.
    """
    lines = readme_path.read_text(encoding="utf-8").splitlines()
    updated_lines: list[str] = []

    for line in lines:
        if line.strip().startswith("|") and is_platform_category_row(line, category_dirname):
            cells = split_markdown_table_row(line)
            if len(cells) >= 4:
                overall_count = parse_int_from_cell(cells[2])
                percentage = format_percentage(documented_count, overall_count)
                line = f"| {cells[0]} | {documented_count} | {overall_count} | {percentage} |"
        updated_lines.append(line)

    documented_total, overall_total = compute_platform_totals(updated_lines)
    total_percentage = format_percentage(documented_total, overall_total)

    final_lines: list[str] = []
    for line in updated_lines:
        if is_total_row(line):
            line = f"| Total | {documented_total} | {overall_total} | {total_percentage} |"
        final_lines.append(line)

    readme_path.write_text("\n".join(final_lines), encoding="utf-8")


# ----------------------------------- Main --------------------------------------


def main() -> None:
    """Traverse `SRC_DIR` and update all README files found."""
    for platform_dirname in list_child_directories(SRC_DIR):
        platform_dir = SRC_DIR / platform_dirname

        for category_dirname in list_child_directories(platform_dir):
            category_dir = platform_dir / category_dirname
            problem_dirnames = list_child_directories(category_dir)

            if not problem_dirnames:
                continue

            # Update each problem README by regenerating its embedded solutions section.
            for problem_dirname in problem_dirnames:
                problem_dir = category_dir / problem_dirname
                solutions_dir = problem_dir / "solutions"

                if not solutions_dir.exists():
                    continue

                solution_filenames = list_child_files(solutions_dir)
                if not solution_filenames:
                    continue

                solutions_markdown = build_problem_solutions_markdown(
                    problem_dir, solution_filenames
                )
                update_problem_readme(problem_dir / README_EN_FILENAME, solutions_markdown)
                update_problem_readme(problem_dir / README_PT_FILENAME, solutions_markdown)

            # Update the category README tables and counts.
            problem_count = len(problem_dirnames)
            table_en = build_category_problem_table_markdown(
                problem_dirnames,
                link_text=SOLUTION_LINK_TEXT_EN,
                readme_filename=README_EN_FILENAME,
            )
            table_pt = build_category_problem_table_markdown(
                problem_dirnames,
                link_text=SOLUTION_LINK_TEXT_PT,
                readme_filename=README_PT_FILENAME,
            )

            update_category_readme(category_dir / README_EN_FILENAME, problem_count, table_en)
            update_category_readme(category_dir / README_PT_FILENAME, problem_count, table_pt)

            # Update platform README to reflect the new category problem count.
            update_platform_readme(platform_dir / README_EN_FILENAME, category_dirname, problem_count)
            update_platform_readme(platform_dir / README_PT_FILENAME, category_dirname, problem_count)


if __name__ == "__main__":
    main()