import pytest


def test_passed():
    pass


def test_failed():
    assert 2 + 2 * 2 == 8


def test_broken():
    raise Exception("Ima broke")


def test_skipped():
    pytest.skip("yo")
    raise Exception("If you see it - ima broke")
