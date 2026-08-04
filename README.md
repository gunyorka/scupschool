# Scup School Quarto course

Scup School is a project-based course for high-school student teams.

The online material prepares students individually for seven personal team
meetings. Each module leads toward a project milestone that the team reaches
during its meeting.

## Course architecture

- 7 pedagogical modules
- 7 personal team meetings
- 7 project milestones
- 1 course-level milestone map
- 1 optional support library
- 1 manifest describing the canonical module and page structure

The expression “learning cycle” is no longer used as a separate architectural
level. The module is the primary pedagogical and navigational unit.

## Important directories

- `course/` — course-wide operational information
- `modules/` — the seven required course modules
- `support/` — optional situation-based support materials
- `assets/styles/` — the shared visual system
- `assets/images/` — course images
- `dev/` — the canonical manifest and development utilities

## Current development status

The project is currently a complete navigational prototype. All canonical
module pages exist with placeholder content. Final page content and recurring
page templates will be developed later.

The canonical page architecture is stored in:

```text
dev/course-manifest.csv
```

## Local preview

From the project root:

```bash
quarto preview
```