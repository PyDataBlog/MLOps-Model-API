try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

try:
    with open('README.rst') as file:
        long_description = file.read()
except IOError:
    long_description = 'Python lib for sniffets.com'

setup(
    name='sniffets',
    packages=['sniffets'],
    version='0.1.8',
    long_description=long_description,
    description='Python lib for sniffets.com',
    author='Doniyor Jurabayev',
    author_email='behconsci@gmail.com',
    url='https://github.com/behconsci/sniffets-python',
    download_url='https://github.com/behconsci/sniffets-python/archive/0.1.8.tar.gz',
    keywords=['track', 'monitor', 'bug'],
    classifiers=[],
    install_requires=[
        'requests', 'grequests'
    ],
)
