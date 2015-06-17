from setuptools import setup

setup(
    name='taurus_nose_plugin',
    py_modules = ['taurus_nose_plugin'],
    entry_points = {
        'nose.plugins': [
            'taurus_nose_plugin = taurus_nose_plugin:TaurusNosePlugin',
            ]
        },
)