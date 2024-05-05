from typing import Any

from jinja2 import Environment, BaseLoader, TemplateSyntaxError, meta
from jinja2.nodes import Template


def validate_syntax(source: str, env: Environment) -> Template | None:
    try:
        return env.parse(source)
    except TemplateSyntaxError:
        return None


def validate_vars(template: Template, availible: set[str]) -> set[str]:
    variables = meta.find_undeclared_variables(template)
    return variables.difference(availible)


def validate(source: str, availible: dict[str, Any]) -> str:
    env = Environment(loader=BaseLoader())
    if not (template := validate_syntax(source, env)):
        return "Invalid syntax"

    if missing := validate_vars(template, set(availible.keys())):
        return f"Missing variables: {', '.join(missing)}"

    return env.from_string(source).render(availible)
