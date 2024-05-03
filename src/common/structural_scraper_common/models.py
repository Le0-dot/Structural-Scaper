from enum import StrEnum
from uuid import uuid4

from pydantic import BaseModel, Field


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


class Document(BaseModel):
    id: str = Field(default_factory=lambda: str(uuid4()), alias="_id")
    host: str
    delay: int
    extractors: list[Extractor]
    template: Template
