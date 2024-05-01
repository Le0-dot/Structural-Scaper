from typing import Any

from fastapi import Request


class DictObject:
    def __init__(self, data: dict[Any, Any] | None = None) -> None:
        super().__setattr__("data", data or self._default())

    def _allowed_attr(self, name: str) -> bool:
        return True

    def _default(self):
        return {}

    def __getattr__(self, name: str) -> Any:
        if not self._allowed_attr(name):
            raise AttributeError(f"No attribute {name} found in class {self.__class__}")
        try:
            return super().__getattribute__("data")[name]
        except KeyError:
            raise AttributeError(f"No attribute {name} found in class {self.__class__}")

    def __setattr__(self, name: str, value: Any) -> None:
        if not self._allowed_attr(name):
            raise AttributeError(f"No attribute {name} found in class {self.__class__}")
        self.__getattribute__("data")[name] = value

    def __delattr__(self, name: str) -> None:
        if not self._allowed_attr(name):
            raise AttributeError(f"No attribute {name} found in class {self.__class__}")
        del self.__getattribute__("data")[name]

    def to_dict(self, **kwargs) -> dict[str, Any]:
        dictionary = {k: v for k, v in self.__getattribute__("data").items() if self._allowed_attr(k)}
        dictionary.update(kwargs)
        return dictionary


class Extractor(DictObject):
    __next_id = 0

    @classmethod
    def __id(cls) -> int:
        cls.__next_id += 1
        return cls.__next_id

    def __init__(self, data: dict[Any, Any] | None = None) -> None:
        if data:
            Extractor.__next_id = max(int(data["id"]), Extractor.__next_id)
        super().__init__(data)

    def _default(self):
        return {
            "id": Extractor.__id(),
            "name": None,
            "selector": None,
            "value": None,
        }

    def _allowed_attr(self, name: str) -> bool:
        return name in ["id", "name", "selector", "value"]

    def valid_value(self) -> bool:
        anchor = self.selector.split()[-1].startswith('a')
        values = ["text", "innerHTML", "outerHTML"]
        if self.value in values or (anchor and self.value == "href"):
            return True
        return False


    def guess_value(self) -> str:
        anchor = self.selector.split()[-1].startswith('a')
        if anchor:
            return "href"
        else:
            return "text"


class Template(DictObject):
    def _default(self):
        return {
            "filename": None,
            "content": "",
        }

    def _allowed_attr(self, name: str) -> bool:
        return name in ["filename", "content"]


class State:
    def __init__(self, request: Request) -> None:
        self.__session = request.session

    def init(self, url: str, delay: int) -> None:
        self.url = url
        self.delay = delay
        self.extractors = []
        self.current_extractor_id = None
        self.template = Template()

    @property
    def url(self) -> str:
        return self.__session["url"]

    @url.setter
    def url(self, value: str) -> None:
        self.__session["url"] = value

    @property
    def delay(self) -> int:
        return self.__session["delay"]

    @delay.setter
    def delay(self, value: int) -> None:
        self.__session["delay"] = value

    @property
    def extractors(self) -> list[dict[Any, Any]]:
        return self.__session["extractors"]

    @extractors.setter
    def extractors(self, values: list[Extractor]) -> None:
        self.__session["extractors"] = values

    @property
    def current_extractor_id(self) -> int | None:
        return self.__session["current_extractor_id"]

    @current_extractor_id.setter
    def current_extractor_id(self, value: int | None) -> None:
        self.__session["current_extractor_id"] = value

    @current_extractor_id.deleter
    def current_extractor_id(self) -> None:
        self.__session["current_extractor_id"] = None

    @property
    def current_extractor(self) -> Extractor | None:
        if self.current_extractor_id is None:
            return None
        return self.extractor_by_id(self.current_extractor_id)

    @current_extractor.deleter
    def current_extractor(self) -> None:
        del self.current_extractor_id

    def extractor_idx_by_id(self, id: int) -> int | None:
        for idx, value in enumerate(self.extractors):
            if value["id"] == id:
                return idx
        return None

    def extractor_by_id(self, id: int) -> Extractor | None:
        idx = self.extractor_idx_by_id(id)
        if idx is None:
            return None
        return Extractor(self.extractors[idx])

    @property
    def template(self) -> Template:
        return Template(self.__session["template"])

    @template.setter
    def template(self, value: Template) -> None:
        self.__session["template"] = value.to_dict()
