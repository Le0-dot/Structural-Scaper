import requests
from urllib.parse import urlparse, unquote

from bs4 import BeautifulSoup as bs, NavigableString
from jinja2 import Environment, FileSystemLoader
from pydantic import BaseModel
from fastapi import FastAPI, Request
from fastapi.responses import FileResponse, HTMLResponse, RedirectResponse
from fastapi.staticfiles import StaticFiles
from starlette.middleware.sessions import SessionMiddleware
from cachetools.func import ttl_cache


app = FastAPI()
app.add_middleware(SessionMiddleware, secret_key='SECRET_KEY')

env = Environment(loader=FileSystemLoader("./templates"))

app.mount('/js', StaticFiles(directory='static/js'), name='js')

def get_url_base(url: str) -> str:
    parsed = urlparse(url)
    return f'{parsed.scheme}://{parsed.netloc}'

def find_css(soup: bs, base: str) -> str:
    links = soup.find_all('link', rel='stylesheet')#type='text/css')
    for link in links:
        if link['href'].startswith('http'):
            continue
        if not link['href'].startswith('/'):
            link['href'] = '/' + link['href']
        link['href'] = base + link['href']
    return '\n'.join(map(str, links))

def remove_js(soup: bs) -> None:
    body = soup.find('body')
    if body is None or isinstance(body, NavigableString):
        return
    for tag in body.find_all('script'):
        tag.decompose()


@ttl_cache(ttl=300)
def get_and_render(url: str) -> str:
    request = requests.get(url)
    soup = bs(request.text, 'html.parser')

    remove_js(soup)

    template = env.get_template("selector.html")
    bindings = {
        'style': soup.find('head style') or '',
        'links': find_css(soup, get_url_base(url)),
        'body': soup.find('body'),
    }

    return template.render(bindings)

class TagSelector(BaseModel):
    tag: str
    id: str | None = None
    classes: list[str] = []

def split_selector(selector: str) -> list[str]:
    result = []
    start_idx = 0
    for idx, char in enumerate(selector):
        if char in '.#':
            result.append(selector[start_idx:idx])
            start_idx = idx
    result.append(selector[start_idx:])
    return result

def parse_tag_selector(selector: str) -> TagSelector:
    values = split_selector(selector)
    tag_selector = TagSelector(tag=values[0])
    for value in values[1:]:
        if value.startswith('#'):
            tag_selector.id = value[1:]
        elif value.startswith('.'):
            tag_selector.classes.append(value[1:])
        else:
            raise Exception('Invalid selector received')
    return tag_selector

def parse_selector(selector: str) -> list[TagSelector]:
    return list(map(parse_tag_selector, selector.split()))


@app.get('/', response_class=FileResponse)
def index(request: Request):
    request.session['step'] = 0
    request.session['selectors'] = []
    print(request.session['step'])
    return './static/html/index.html'

@app.get('/select', response_class=HTMLResponse)
def select(request: Request, url: str):
    print(request.session['step'])
    url = unquote(url)
    request.session['url'] = url
    return get_and_render(url)

@app.get('/select/details', response_class=HTMLResponse)
def details(selector: str):
    selectors = parse_selector(selector)
    template = env.get_template('details.html')
    return template.render({'selectors': selectors})

@app.get('/select/next', response_class=RedirectResponse)
def next(request: Request, selector: str):
    request.session['step'] += 1
    request.session['selectors'].append(selector)
    print(request.session['step'])
    if request.session['step'] < 3:
        return f'/select?url={request.session["url"]}'
    else:
        return f'/select/confirm'


@app.get('/select/confirm', response_class=HTMLResponse)
def confirm(request: Request):
    return f'''
    {request.session['selectors']}
    '''
