include .env
export

.PHONY: watch migrate up down

watch:
	./watch.sh

migrate:
	gleam run -m cigogne all

up:
	gleam run -m cigogne up

down:
	gleam run -m cigogne down
