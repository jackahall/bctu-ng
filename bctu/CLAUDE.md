# bctu package — development guidance

Rebuild of the BCTU clinical-trial data-management R package. Regulated context:
UK MHRA / EMA / FDA, GCP. Rebuild spec (the design of record):
`~/projects/bctu/BCTU_REVIEW_AND_REBUILD_PLAN.md`.

## Who uses this (this drives every API decision)
Most users are **NOT R experts and not regular R users** — trial statisticians and
data managers who use bctu occasionally. Therefore:

- **Function and argument names must be clear, obvious, and self-explanatory.**
  A non-R user should be able to guess what a function does from its name. No
  R-insider jargon, no cute metaphors. For example, the environment/setup check
  is `check_setup()`, NOT `bctu_doctor` ("doctor" is insider slang and unclear).
- **The happy path must be obvious**; the common task should be one obvious call.
- **Errors are plain English and actionable** (say what is wrong and what to do),
  never a bare R condition.
- Prefer clear verbs for actions: `take_snapshot`, `load_snapshot`,
  `delete_snapshot`, `check_setup`, `save_dvr`. Avoid abbreviations that are not
  immediately obvious.

## Coding rules (in addition to the global ~/.claude/CLAUDE.md)
- **No hidden helpers**: never a `.dot`-prefixed or otherwise concealed function.
  Every function is explicitly named so all logic is visible and followable.
- **Explicit configuration, no ambient state**: locations, tokens and connections
  are declared (project marker `bctu-project.yml`, `credential_spec`), never
  discovered from global options or the working directory. Resolved paths are
  announced; when a location is ambiguous, error loudly rather than guess.
- **Records are human-readable and machine-readable** (GCP): manifests and the
  audit ledger are plain-text YAML, never binary. Binary/typed formats are for
  the data payload only, never the metadata.
- **Auditable and reproducible**: every extraction and deletion is recorded in the
  append-only audit ledger; integrity is SHA-256 in the manifest; provenance is
  independent of the trial git repo.
- One canonical time policy (UTC snapshot ids, explicit-timezone data-cut dates);
  never format or parse a snapshot time by hand.

## Layout
- `R/` package code; one file per subsystem (config, datasource, snapshot,
  checks, report, validation).
- Superfolder `~/projects/bctu-ng/` is the workspace; the package is `bctu/`.
- The old package `~/projects/bctu/bctu` is the reference and is left untouched.
