from jinja2 import Environment, FileSystemLoader
from selenium import webdriver


def resource(factory):
    class Resource:
        __value = None

        @classmethod
        def get(cls):
            if cls.__value is None:
                cls.__value = factory()
            yield cls.__value

    return Resource


Env = resource(lambda: Environment(loader=FileSystemLoader("./templates")))
Driver = resource(lambda: webdriver.Firefox())
