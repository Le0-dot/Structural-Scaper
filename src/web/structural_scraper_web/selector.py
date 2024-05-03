from typing import Any, Iterable

from pydantic import BaseModel


class TagSelector(BaseModel):
    tag: str
    id: str | None = None
    classes: list[str] = []


def split(selector: str) -> list[str]:
    result = []
    start_idx = 0
    for idx, char in enumerate(selector):
        if char in ".#":
            result.append(selector[start_idx:idx])
            start_idx = idx
    result.append(selector[start_idx:])
    return result


def parse_tag_selector(selector: str) -> TagSelector:
    values = split(selector)
    tag_selector = TagSelector(tag=values[0])
    for value in values[1:]:
        if value.startswith("#"):
            tag_selector.id = value[1:]
        elif value.startswith("."):
            tag_selector.classes.append(value[1:])
        else:
            raise Exception("Invalid selector received")
    return tag_selector


def parse_selector(selector: str) -> list[TagSelector]:
    return list(map(parse_tag_selector, selector.split()))


def find_by_idx(idx: int, sequence: Iterable[tuple[int, Any]]) -> Any | None:
    for i, value in sequence:
        if i == idx:
            return value
    return None


def join_list(sep: str, value: Any) -> str:
    if isinstance(value, list):
        return sep.join(value)
    return value


def sorted_by_first(values: list[tuple[Any, Any]]) -> list[Any]:
    return list(zip(*sorted(values, key=lambda x: x[0])))[1]


def build_selector(parts: dict[str, str | list[str]]) -> str:
    tags, ids, classes = [], [], []
    for part in parts:
        match part.split("-"):
            case ["tag", idx]:
                tags.append((int(idx), part))
            case ["id", idx]:
                ids.append((int(idx), part))
            case ["class", idx]:
                classes.append((int(idx), part))

    selector_parts = []
    for idx, tag in tags:
        part = parts[tag]
        if id_idx := find_by_idx(idx, ids):
            part += f"#{parts[id_idx]}"
        if clses_idx := find_by_idx(idx, classes):
            part += f".{join_list('.', parts[clses_idx])}"
        selector_parts.append((idx, part))

    return " ".join(sorted_by_first(selector_parts))
