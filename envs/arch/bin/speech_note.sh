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
  echo "  -r            Remove original files after processing."
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

# System prompt
SYSTEM_PROMPT="以下のテキストは、音声入力されたものであり、フィラー（例: 'えー', 'あー'）や話し言葉特有の表現、音声認識の誤りが含まれている可能性があります。これらを適切に修正し、正式な書き言葉として自然な文章に整形してください。また、内容を日本語として明瞭にし、適切な見出しを付けた${FORMAT}形式のアウトラインを作成してください。入力文章の変換結果以外はレスポンスに一切含めないでください。"

# Process text files in the input directory
for file in "$INPUT_DIR"/*.txt; do
  if [[ -f "$file" ]]; then
    echo "Processing file: $file" >&2

    # Read file content
    content=$(<"$file")

    # Ollama API request
    response=$(curl -s -X POST "http://${OLLAMA_HOST}:${OLLAMA_PORT}/api/generate" \
      -H 'Content-Type: application/json' \
      -d '{
        "model": "'"$MODEL_NAME"'",
        "prompt": "'"$content"'",
        "system": "'"$SYSTEM_PROMPT"'",
        "stream": false
      }')

    # Extract the 'response' field from the JSON
    formatted_content=$(echo "$response" | jq -r '.response')

    # Output the formatted content
    echo "$formatted_content"

    # Remove the original file if -r option is specified
    if [ "$REMOVE_ORIGINAL" = true ]; then
      rm "$file"
      echo "Removed original file: $file" >&2
    fi
  else
    echo "No text files found in $INPUT_DIR" >&2
  fi
done
