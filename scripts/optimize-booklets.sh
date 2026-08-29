#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
src_dir="$repo_root/assets/booklets"
out_dir="$src_dir/web"

if ! command -v gs >/dev/null 2>&1; then
  echo "ghostscript (gs) not found -- cannot build the web copies." >&2
  exit 1
fi

mkdir -p "$out_dir"

shopt -s nullglob
sources=("$src_dir"/*.pdf)

if [[ ${#sources[@]} -eq 0 ]]; then
  echo "No booklet PDFs found in assets/booklets/ -- nothing to do."
  exit 0
fi

for src in "${sources[@]}"; do
  name="$(basename "$src")"
  out="$out_dir/$name"

  if [[ -f "$out" && "$out" -nt "$src" ]]; then
    echo "up to date  $name"
    continue
  fi

  tmp="$(mktemp -t booklet-XXXXXX.pdf)"

  gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.7 -dNOPAUSE -dQUIET -dBATCH \
     -dDetectDuplicateImages=true -dCompressFonts=true -dSubsetFonts=true \
     -dDownsampleColorImages=true -dColorImageResolution=120 -dColorImageDownsampleType=/Bicubic \
     -dDownsampleGrayImages=true -dGrayImageResolution=120 -dGrayImageDownsampleType=/Bicubic \
     -dDownsampleMonoImages=true -dMonoImageResolution=300 \
     -dAutoFilterColorImages=false -dColorImageFilter=/DCTEncode \
     -dAutoFilterGrayImages=false -dGrayImageFilter=/DCTEncode \
     -dJPEGQ=72 -dPassThroughJPEGImages=false \
     -sOutputFile="$tmp" "$src"

  if command -v qpdf >/dev/null 2>&1; then
    qpdf --linearize "$tmp" "$out" || cp "$tmp" "$out"
  else
    cp "$tmp" "$out"
  fi

  rm -f "$tmp"

  before="$(du -h "$src" | cut -f1)"
  after="$(du -h "$out" | cut -f1)"
  echo "built       $name  ($before -> $after)"
done
