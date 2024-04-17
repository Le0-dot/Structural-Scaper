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
