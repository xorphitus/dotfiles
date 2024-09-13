#!/usr/bin/env bash

# Restart Ollama to release video memory
# otherwise hibernation will probably fail.
systemctl --user restart ollama

systemctl hibernate
