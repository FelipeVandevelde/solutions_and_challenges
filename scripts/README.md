##### Este é o Readme do projeto _em inglês_, caso prefira acompanhar o Readme em Português-BR, [clique aqui](./README-PT.md)
# Scripts

| Script | Code | Explanation |
| --- | --- | --- | 
| `update_readmes.py` | [View code](update_readmes.py) | [View explanation](#update_readmespy) |

---

## `update_readmes.py`

Auto-updates README files under `./src` by recreating tables, counters, and embedded solution code blocks.

### What it updates

- **Problem READMEs** (`README.md` and `README-PT.md` inside each problem folder)
  - Replaces everything after the marker line `<!-- solutions -->` with generated Markdown that embeds the contents of each file in the `solutions/` folder.
- **Category READMEs** (`README.md` and `README-PT.md` inside each category folder)
  - Updates the H1 header counter that matches `# <title> (N/M)` by replacing **N** with the number of problem directories found.
  - Replaces the table body after the separator row `|---|---|---|` with a freshly generated list of problems and links.
- **Platform READMEs** (`README.md` and `README-PT.md` inside each platform folder)
  - Updates the row for each category with the new documented count and percentage.
  - Recomputes the **Total** row by summing all categories.

### Expected repository structure

The script assumes this layout:

- `src/<platform>/<category>/<problem>/`
  - `README.md`
  - `README-PT.md`
  - `solutions/` (directory containing one or more solution files)

Problem directories are expected to be named like:

- `<id> - <problem name>` (example: `1000 - Hello World!`)

### Required README markers

- **In problem READMEs**: the line `<!-- solutions -->` must exist.
- **In category READMEs**: the Markdown table separator row `|---|---|---|` must exist.
- **In category READMEs**: the first H1 header must match `# <anything> (N/M)` so the script can update **N**.

### Trigger Description

The script starts automatically whenever the following conditions are met:
- **Changes in the `src` directory**: When there is a new _commit_ that modifies files within the `src` folder, except for the `README.md` and `README-PT.md` files.
- **Changes to the Script**: When there are direct changes to the script file itself.

### Notes

- The script overwrites content **after** the markers described above. Make sure you keep the markers in place.
- Directory and file lists are sorted to keep the generated output deterministic.