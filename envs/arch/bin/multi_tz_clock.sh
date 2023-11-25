#!/usr/bin/env bash
tokyo=$(TZ=Asia/Tokyo date +"%H:%M")
la=$(TZ=America/Los_Angeles date +"%H:%M")
utc=$(date -u +"%H:%M")

echo "(JP) ${tokyo} (LA) ${la} (UTC) ${utc}"
