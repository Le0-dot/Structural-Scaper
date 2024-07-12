db := testDB.db
tailwind_config := static/tailwind.config.js
tailwind_input := static/tailwind.css
tailwind_output := static/src/css/style.css

all: run

init-db:
	sqlite3 $(db) < init.sql

tailwind:
	tailwindcss -c $(tailwind_config) -i $(tailwind_input) -o $(tailwind_output)

run: init-db tailwind
	cabal run

download-tailwind:
	wget https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.4/tailwindcss-linux-x64 -O tailwindcss
	chmod +x tailwindcss

rm-db:
	rm $(db)

rm-tailwind:
	rm $(tailwind_output)

clean: rm-db rm-tailwind
	cabal clean
