from fastapi.templating import Jinja2Templates
from selenium.webdriver import Remote, FirefoxOptions
from selenium.webdriver.remote.webdriver import WebDriver


def templates(path: str = "templates") -> Jinja2Templates:
    return Jinja2Templates(path, autoescape=False, auto_reload=True)

def driver(url: str = "http://127.0.0.1:4444") -> WebDriver:
    return Remote(command_executor=url, options=FirefoxOptions())
