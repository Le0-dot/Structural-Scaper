db := testDB.db

all: run

init:
	sqlite3 $(db) < init.sql

run: init
	cabal run

rm-db:
	rm $(db)

clean: rm-db
	cabal clean
