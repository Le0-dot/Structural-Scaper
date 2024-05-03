from enum import StrEnum
from typing import Any, Annotated

from pydantic import BaseModel, Field, BeforeValidator


class ValueType(StrEnum):
    href = "href"
    text = "text"
    innerHTML = "innerHTML"
    outerHTML = "outerHTML"


class Extractor(BaseModel):
    name: str
    selector: str
    value: ValueType


class Template(BaseModel):
    filename: str
    content: str
    next: str


ObjectId = Annotated[str, BeforeValidator(str)]


class Document(BaseModel):
    id: ObjectId | None = Field(default=None, alias="_id")
    host: str
    delay: int
    extractors: list[Extractor]
    template: Template

    def dump(self) -> dict[str, Any]:
        return self.model_dump(by_alias=True, exclude=["id"])
