from rever.activities.ghrelease import git_archive_asset


$PROJECT = $GITHUB_ORG = $GITHUB_REPO = 'xonsh'
$WEBSITE_URL = 'http://xon.sh'
$ACTIVITIES = ['authors', 'version_bump', 'changelog', 'pytest', 'appimage',
               'tag', 'push_tag',
               'ghrelease',
               'sphinx',
               'ghpages',
               'pypi',
               'conda_forge',
               ]
$PYPI_SIGN = False

$AUTHORS_FILENAME = "AUTHORS.rst"
$VERSION_BUMP_PATTERNS = [
    ('xonsh/__init__.py', r'__version__\s*=.*', '__version__ = "$VERSION"'),
    ]
$CHANGELOG_FILENAME = 'CHANGELOG.rst'
$CHANGELOG_TEMPLATE = 'TEMPLATE.rst'

$PYTEST_COMMAND = "./run-tests.xsh -- test"

$TAG_REMOTE = 'git@github.com:xonsh/xonsh.git'
$TAG_TARGET = 'master'

$GHPAGES_REPO = 'git@github.com:xonsh/xonsh-docs.git'

$DOCKER_APT_DEPS = ['man']
with open('requirements/tests.txt') as f:
    conda_deps = f.read().split()
with open('requirements/docs.txt') as f:
    conda_deps += f.read().split()
for delimiter in '=<>':
    conda_deps = {d.lower().split(delimiter)[0] for d in conda_deps}
conda_deps.discard('prompt-toolkit')
conda_deps |= {'prompt_toolkit', 'pip', 'psutil', 'numpy', 'matplotlib'}
$DOCKER_CONDA_DEPS = sorted(conda_deps)
$DOCKER_INSTALL_COMMAND = ('rm -rf .cache/ __pycache__/ */__pycache__ */*/__pycache__ build/ && '
                           './setup.py install')
$DOCKER_GIT_NAME = 'xonsh'
$DOCKER_GIT_EMAIL = 'xonsh@googlegroups.com'

$GHRELEASE_ASSETS = [git_archive_asset, 'xonsh-x86_64.AppImage']

$APPIMAGE_PYTHON_VERSION = '3.8'
