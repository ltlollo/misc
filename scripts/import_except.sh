#!/usr/bin/env bash

panic() {
    echo "$@"; exit 1
}

args_or_die() {
    [[ "$1" ]] || panic "error: no args"
}

