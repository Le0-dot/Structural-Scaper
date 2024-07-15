db := testDB.db
tailwind_config := static/tailwind.config.js
tailwind_input := static/tailwind.css
tailwind_output := static/src/css/style.css
htmx_dir := static/src/js/htmx

all: run

init-db:
	sqlite3 $(db) < init.sql

tailwind:
	./tailwindcss -c $(tailwind_config) -i $(tailwind_input) -o $(tailwind_output) --minify

run: rm-db init-db tailwind
	cabal run

build:
	cabal build

download-htmx:
	mkdir -p $(htmx_dir)
	wget https://unpkg.com/htmx.org@2.0.1/dist/htmx.min.js -P $(htmx_dir)
	wget https://unpkg.com/htmx.org@2.0.1/dist/ext/json-enc.js -P $(htmx_dir)

download-tailwind-executable:
	wget https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.4/tailwindcss-linux-x64 -O tailwindcss
	chmod +x tailwindcss

rm-db:
	rm -f $(db)

rm-htmx:
	rm -rf $(htmx_dir)

rm-tailwind:
	rm $(tailwind_output)

rm-tailwind-executalbe:
	rm tailwindcss

init: download-htmx download-tailwind-executable

clean: rm-db rm-htmx rm-tailwind
	cabal clean
