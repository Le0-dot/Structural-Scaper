from jinja2 import Environment, TemplateSyntaxError, meta
from jinja2.nodes import Template


def validate_syntax(source: str) -> Template | None:
    try:
        return Environment().parse(source)
    except TemplateSyntaxError:
        return None


def validate_vars(template: Template, availible: set[str]) -> set[str]:
    variables = meta.find_undeclared_variables(template)
    return variables.difference(availible)


def validate(source: str, availible: set[str]) -> str:
    if not (template := validate_syntax(source)):
        return "Invalid syntax"

    if missing := validate_vars(template, availible):
        return f"Missing variables {', '.join(missing)}"

    return "Template is correct"
