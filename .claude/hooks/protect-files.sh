#!/bin/bash
# Block accidental edits to protected files
# Customize PROTECTED_PATTERNS below for your project
INPUT=$(cat)
TOOL=$(echo "$INPUT" | jq -r '.tool_name')
FILE=""

# Extract file path based on tool type
if [ "$TOOL" = "Edit" ] || [ "$TOOL" = "Write" ]; then
  FILE=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty')
fi

# No file path = not a file operation, allow
if [ -z "$FILE" ]; then
  exit 0
fi

# ============================================================
# CUSTOMIZE: Add patterns for files you want to protect
# Uses basename matching — add full paths for more precision
# ============================================================
PROTECTED_PATTERNS=(
  "Bibliography_base.bib"
  "settings.json"
)

# Protect entire directories — block any write/edit inside these paths
PROTECTED_DIRS=(
  "Project/Data/Data_Raw"
  "Project/Overleaf/Original_Files"
)

for DIR in "${PROTECTED_DIRS[@]}"; do
  if [[ "$FILE" == *"$DIR"* ]]; then
    echo "Protected directory: $DIR is read-only. Save new/modified files to Project/Overleaf/Update_Files instead." >&2
    exit 2
  fi
done

BASENAME=$(basename "$FILE")
for PATTERN in "${PROTECTED_PATTERNS[@]}"; do
  if [[ "$BASENAME" == "$PATTERN" ]]; then
    echo "Protected file: $BASENAME. Edit manually or remove protection in .claude/hooks/protect-files.sh" >&2
    exit 2
  fi
done

exit 0
