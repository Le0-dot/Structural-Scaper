from urllib.parse import unquote

from bs4 import BeautifulSoup as bs
from jinja2 import Environment, FileSystemLoader
from fastapi import FastAPI, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from starlette.middleware.sessions import SessionMiddleware

import state
from .selector import parse_selector
from .cleaner import get_and_render

app = FastAPI()
app.add_middleware(SessionMiddleware, secret_key='SECRET_KEY') # Change to random string

env = Environment(loader=FileSystemLoader("./templates"))

app.mount('/js', StaticFiles(directory='static/js'), name='js')



@app.get('/', response_class=FileResponse)
def index(request: Request):
    state.init(request.session)
    return './static/html/index.html'

@app.get('/select', response_class=HTMLResponse)
def select(request: Request, url: str):
    url = unquote(url)
    state.select(request.session, url)
    return get_and_render(url, env.get_template('selector.html'))

@app.get('/select/details', response_class=HTMLResponse)
def details(selector: str):
    selectors = parse_selector(selector)
    template = env.get_template('details.html')
    return template.render({'selectors': selectors})

@app.get('/select/next', response_class=RedirectResponse)
def next(request: Request, selector: str):
    if state.push(request.session, selector):
        return f'/select?url={state.get_url(request.session)}'
    else:
        return f'/select/confirm'


@app.get('/select/confirm', response_class=HTMLResponse)
def confirm(request: Request):
    return f'''
    {request.session['selectors']}
    '''
