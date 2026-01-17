#!/bin/bash
fswatch -o src test | while read; do gleam test; done
