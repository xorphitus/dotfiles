#!/usr/bin/env bash

# Default values
INPUT_DIR=""
MODEL_NAME="elyza:llama3-jp-8b"
FORMAT="org-mode"
REMOVE_ORIGINAL=false
OLLAMA_HOST="localhost"
OLLAMA_PORT="11434"

# Function to display usage
function usage {
  echo "Usage: $0 [-r] [-f format] [-m model] [-H host] [-p port] directory"
  echo "  -r            Remove original files after processing without prompt."
  echo "  -f format     Specify the output format (default: org-mode)."
  echo "  -m model      Specify the model name for Ollama (default: qwen2.5:3b)."
  echo "  -H host       Specify the Ollama host (default: localhost)."
  echo "  -p port       Specify the Ollama port (default: 11434)."
  echo "  directory     Directory containing text files to process."
  exit 1
}

# Parse options
while getopts "rf:m:H:p:" opt; do
  case ${opt} in
    r)
      REMOVE_ORIGINAL=true
      ;;
    f)
      FORMAT=$OPTARG
      ;;
    m)
      MODEL_NAME=$OPTARG
      ;;
    H)
      OLLAMA_HOST=$OPTARG
      ;;
    p)
      OLLAMA_PORT=$OPTARG
      ;;
    *)
      usage
      ;;
  esac
done
shift $((OPTIND -1))

# Check if directory is provided
if [ $# -ne 1 ]; then
  usage
fi

INPUT_DIR=$1

# Check if input directory exists
if [ ! -d "$INPUT_DIR" ]; then
  echo "Error: Directory '$INPUT_DIR' does not exist."
  exit 1
fi

# System prompt
SYSTEM_PROMPT="以下のテキストは、音声入力されたものであり、フィラー（例: 'えー', 'あー'）や話し言葉特有の表現、音声認識の誤りが含まれている可能性があります。これらを適切に修正し、正式な書き言葉として自然な文章に整形してください。また、内容を要約し、適切な見出しを付けた${FORMAT}形式のアウトラインを作成してください。出力は${FORMAT}形式のアウトラインのみを含み、他の説明やコメントは不要です。見出し文を強調してはいけません。箇条書きに見出し文と同じ文字を使ってもいけません。"

# Temporary file to hold concatenated content
TEMP_FILE=$(mktemp)

# Concatenate all .txt files in the input directory
for file in "$INPUT_DIR"/*.txt; do
  if [[ -f "$file" ]]; then
    cat "$file" >> "$TEMP_FILE"
  fi
done

# Check if TEMP_FILE is not empty
if [ ! -s "$TEMP_FILE" ]; then
  echo "No text files found in $INPUT_DIR"
  rm "$TEMP_FILE"
  exit 1
fi

# Convert content to a JSON-safe string
content_json=$(jq -Rs . < "$TEMP_FILE")

# Ollama API request
response=$(curl -s -X POST "http://${OLLAMA_HOST}:${OLLAMA_PORT}/api/generate" \
  -H 'Content-Type: application/json' \
  -d '{
    "model": "'"$MODEL_NAME"'",
    "prompt": '"$content_json"',
    "system": "'"$SYSTEM_PROMPT"'",
    "stream": false
  }')

# Extract the 'response' field from the JSON
formatted_content=$(echo "$response" | jq -r '.response')

# Output the formatted content
echo "$formatted_content"

# Remove the temporary file
rm "$TEMP_FILE"

# Function to prompt for file deletion
prompt_for_deletion() {
  read -p "Do you want to delete the original files? (y/N): " confirm
  case "$confirm" in
    [yY][eE][sS]|[yY])
      for file in "$INPUT_DIR"/*.txt; do
        if [[ -f "$file" ]]; then
          rm "$file"
          echo "Removed original file: $file" >&2
        fi
      done
      ;;
    *)
      echo "Original files were not deleted." >&2
      ;;
  esac
}

# Remove original files if -r option is specified, otherwise prompt
if [ "$REMOVE_ORIGINAL" = true ]; then
  for file in "$INPUT_DIR"/*.txt; do
    if [[ -f "$file" ]]; then
      rm "$file"
      echo "Removed original file: $file" >&2
    fi
  done
else
  prompt_for_deletion
fi
