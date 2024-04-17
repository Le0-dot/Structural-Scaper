from typing import Any


def init(session: dict[str, Any]) -> None:
    session["step"] = 0
    session["selectors"] = []


def select(session: dict[str, Any], url: str) -> None:
    session["url"] = url


def push(session: dict[str, Any], selector: str) -> bool:
    session["step"] += 1
    session["selectors"].append(selector)
    return session["step"] < 3


def get_url(session: dict[str, Any]) -> str:
    return session["url"]


def get_selectors(session: dict[str, Any]) -> list[str]:
    return session["selectors"]
