DROP TABLE recipe;
DROP TABLE extractor;
DROP TABLE template;
DROP TABLE extractor_draft;
DROP TABLE selector;
DROP TABLE selector_class;

CREATE TABLE recipe (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR NOT NULL,
    host VARCHAR NOT NULL,
    delay_ms INTEGER NOT NULL,
    is_draft BOOLEAN NOT NULL CHECK (is_draft IN (0, 1))
);
CREATE TABLE extractor (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR NOT NULL,
    selector VARCHAR NOT NULL,
    type VARCHAR NOT NULL,
    recipe INTEGER NOT NULL,
    FOREIGN KEY(recipe) REFERENCES recipe(id)
);
CREATE TABLE template (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    filename VARCHAR NOT NULL,
    content VARCHAR NOT NULL,
    next VARCHAR NOT NULL,
    recipe INTEGER NOT NULL,
    FOREIGN KEY(recipe) REFERENCES recipe(id)
);
CREATE TABLE extractor_draft (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR NOT NULL,
    type VARCHAR,
    recipe INTEGER NOT NULL,
    FOREIGN KEY(recipe) REFERENCES recipe(id)
);
CREATE TABLE selector (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    tag VARCHAR NOT NULL,
    tag_id VARCHAR,
    tag_id_is_taken BOOLEAN NOT NULL CHECK (tag_id_is_taken IN (0, 1)),
    extractor_draft INTEGER NOT NULL,
    FOREIGN KEY(extractor_draft) REFERENCES extractor_draft(id)
);
CREATE TABLE selector_class (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    value VARCHAR NOT NULL,
    is_taken BOOLEAN NOT NULL CHECK (is_taken IN (0, 1)),
    selector INTEGER NOT NULL,
    FOREIGN KEY(selector) REFERENCES selector(id)
);
