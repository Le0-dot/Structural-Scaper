from random import randint
from enum import StrEnum

from pydantic import BaseModel, Field, computed_field, model_validator


class ValueType(StrEnum):
    href = "href"
    text = "text"
    innerHTML = "innerHTML"
    outerHTML = "outerHTML"


class Extractor(BaseModel):
    id: int = Field(
        default_factory=lambda: randint(0, 2**32 - 1)
    )  # random id, so that we do not need to keep incrementing value around
    name: str | None = None
    selector: str | None = None
    value: ValueType | None = None
    regex: str | None = None

    @model_validator(mode="after")
    def valid_value(self) -> "Extractor":
        if not self.anchor_selector and self.value == ValueType.href:
            self.value = self.guess_value()
        return self

    @computed_field
    @property
    def anchor_selector(self) -> bool:
        if self.selector is None:
            return False
        return self.selector.split()[-1].startswith("a")

    def guess_value(self) -> ValueType:
        if self.anchor_selector:
            return ValueType.href
        else:
            return ValueType.text


class Template(BaseModel):
    filename: str | None = None
    content: str = ""


class State(BaseModel):
    url: str
    delay: int
    extractors: dict[int, Extractor] = {}
    current_extractor_id: int | None = None
    current_selector: str | None = None
    template: Template = Template()

    @property
    def current_extractor(self) -> Extractor | None:
        if self.current_extractor_id is None:
            return None
        return self.extractors[self.current_extractor_id]

    @current_extractor.deleter
    def current_extractor(self) -> None:
        self.current_extractor_id = None

    def append_extractor(self, extractor: Extractor) -> None:
        if extractor.id in self.extractors.keys():
            raise ValueError("trying to append already existing extractor")
        self.extractors[extractor.id] = extractor

    @property
    def names(self) -> set[str]:
        return {
            extractor.name
            for extractor in self.extractors.values()
            if extractor.name is not None
        }
