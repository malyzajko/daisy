#!/bin/bash

set -e
cargo build -r && cp ./target/release/omelette ../lib/omelette
