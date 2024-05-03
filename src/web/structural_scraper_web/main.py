from random import randint

from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from starlette.middleware.sessions import SessionMiddleware

from routers import *


app = FastAPI()

app.include_router(index_router, prefix="")
app.include_router(recipe_router, prefix="/recipe")
app.include_router(extractor_router, prefix="/recipe/extractor")
app.include_router(template_router, prefix="/recipe/template")
app.include_router(select_router, prefix="/select")
app.include_router(details_router, prefix="/select/details")

app.mount("/js", StaticFiles(directory="static/js"), name="js")
app.mount("/css", StaticFiles(directory="static/css"), name="css")


def gen_secret(length: int) -> str:
    return "".join([chr(randint(0, 128)) for _ in range(length)])


app.add_middleware(SessionMiddleware, secret_key="sdfadsfa")  # gen_secret(128))
