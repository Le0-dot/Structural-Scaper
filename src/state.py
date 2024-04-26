from typing import Any

from fastapi import Request


class DictObject:
    def __init__(self, data: dict[Any, Any]) -> None:
        super().__setattr__("data", data)

    def _allowed_attr(self, name: str) -> bool:
        return True

    def __getattr__(self, name: str) -> str:
        if self._allowed_attr(name):
            raise AttributeError(f'No attribute {name} found in class {self.__class__}')
        try:
            return super().__getattribute__("data")[name]
        except KeyError:
            raise AttributeError(f'No attribute {name} found in class {self.__class__}')

    def __setattr__(self, name: str, value: Any) -> None:
        if self._allowed_attr(name):
            raise AttributeError(f'No attribute {name} found in class {self.__class__}')
        super().__getattribute__("data").__setitem__(name, value)


class Extractor(DictObject):
    def _allowed_attr(self, name: str) -> bool:
        return name in ['name', 'selector']


class State:
    def __init__(self, request: Request) -> None:
        self.__session = request.session

    def init(self, url: str, delay: int) -> None:
        self.url = url
        self.delay = delay
        self.extractors = []
        self.current_extractor_idx = None

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
    def extractors(self) -> list[Extractor]:
        return list(map(Extractor, self.__session["extractors"]))

    @extractors.setter
    def extractors(self, values: list[Extractor]) -> None:
        self.__session["extractors"] = [do.data for do in values]

    @property
    def current_extractor_idx(self) -> int | None:
        return self.__session["current_extractor_idx"]

    @current_extractor_idx.setter
    def current_extractor_idx(self, value: int | None) -> None:
        self.__session["current_extractor_idx"] = value

    @current_extractor_idx.deleter
    def current_extractor_idx(self) -> None:
        self.__session["current_extractor_idx"] = None

    @property
    def current_extractor(self) -> Extractor | None:
        idx = self.current_extractor_idx
        return Extractor(self.__session["extractors"][idx])

    @current_extractor.setter
    def current_extractor(self, value: Extractor) -> None:
        idx = self.current_extractor_idx
        self.__session["extractors"][idx] = value.data

    @current_extractor.deleter
    def current_extractor(self) -> None:
        del self.current_extractor_idx
